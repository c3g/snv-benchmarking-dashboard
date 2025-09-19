# ============================================================================
# auth.py - Enhanced with Session Management
# ============================================================================
"""
authentication with proper session management and PostgreSQL compatibility.
"""

import hashlib
import secrets
import logging
from user_models import User, UserRole
from database import get_db_session
from sqlalchemy.exc import IntegrityError, OperationalError
from contextlib import contextmanager
import time

logger = logging.getLogger(__name__)

# ============================================================================
# ENHANCED PASSWORD MANAGEMENT
# ============================================================================

def hash_password(password):
    """Hash password using SHA256 + salt"""
    salt = secrets.token_hex(16)
    password_hash = hashlib.sha256((password + salt).encode()).hexdigest()
    return f"{salt}:{password_hash}"

def verify_password(password, stored_hash):
    """Verify password against stored hash"""
    try:
        salt, password_hash = stored_hash.split(':')
        test_hash = hashlib.sha256((password + salt).encode()).hexdigest()
        return test_hash == password_hash
    except:
        return False

# ============================================================================
# ENHANCED SESSION MANAGEMENT
# ============================================================================

@contextmanager
def get_auth_session():
    """
    Dedicated session manager for authentication operations.
    Handles database retries and connection issues.
    """
    max_retries = 3
    retry_delay = 1
    
    for attempt in range(max_retries):
        try:
            with get_db_session() as session:
                yield session
                return
        except OperationalError as e:
            logger.warning(f"Database connection attempt {attempt + 1} failed: {e}")
            if attempt < max_retries - 1:
                time.sleep(retry_delay)
                retry_delay *= 2  # Exponential backoff
            else:
                raise

# ============================================================================
# CORE USER FUNCTIONS - Enhanced
# ============================================================================

def create_user(username, email, password, role='user'):
    """Create a new user account with enhanced error handling"""
    try:
        with get_auth_session() as session:
            # Normalize inputs
            username = username.strip()
            email = email.strip().lower()
            
            # Check if user exists
            existing = session.query(User).filter(
                (User.username == username) | (User.email == email)
            ).first()
            
            if existing:
                if existing.username == username:
                    return {"success": False, "message": "Username already exists", "code": "USERNAME_EXISTS"}
                else:
                    return {"success": False, "message": "Email already registered", "code": "EMAIL_EXISTS"}
            
            # Validate role
            user_role = UserRole.ADMIN if role.lower() == 'admin' else UserRole.USER
            
            # Create user
            new_user = User(
                username=username,
                email=email,
                password_hash=hash_password(password),
                role=user_role,
                is_active=True
            )
            
            session.add(new_user)
            session.flush()  # Get ID without committing
            user_id = new_user.id
            
            logger.info(f"User created: {username} (ID: {user_id}, Role: {user_role.value})")
            return {
                "success": True, 
                "user_id": user_id,
                "username": username,
                "role": user_role.value
            }
            
    except IntegrityError as e:
        logger.error(f"Integrity error creating user {username}: {e}")
        return {"success": False, "message": "Username or email already exists", "code": "INTEGRITY_ERROR"}
    except Exception as e:
        logger.error(f"Error creating user {username}: {e}")
        return {"success": False, "message": f"Database error: {str(e)}", "code": "DATABASE_ERROR"}

def authenticate_user(username, password):
    """Enhanced authentication with better error handling"""
    try:
        with get_auth_session() as session:
            # Try to find user by username or email
            user = session.query(User).filter(
                (User.username == username.strip()) | (User.email == username.strip().lower()),
                User.is_active == True
            ).first()
            
            if not user:
                logger.warning(f"Authentication failed: User '{username}' not found")
                return {"success": False, "message": "Invalid credentials", "code": "USER_NOT_FOUND"}
            
            # Verify password
            if not verify_password(password, user.password_hash):
                logger.warning(f"Authentication failed: Invalid password for user '{username}'")
                return {"success": False, "message": "Invalid credentials", "code": "INVALID_PASSWORD"}
            
            # Authentication successful
            user_data = {
                "id": user.id,
                "username": user.username,
                "email": user.email,
                "role": user.role.value,
                "created_at": user.created_at.isoformat() if user.created_at else None
            }
            
            logger.info(f"User authenticated: {user.username} (ID: {user.id})")
            return {
                "success": True,
                "user": user_data,
                "session_token": generate_session_token(user.id)  # For future session management
            }
            
    except Exception as e:
        logger.error(f"Authentication error for '{username}': {e}")
        return {"success": False, "message": "Authentication system error", "code": "SYSTEM_ERROR"}

def get_user_by_id(user_id, include_inactive=False):
    """Enhanced user retrieval with better error handling"""
    try:
        with get_auth_session() as session:
            query = session.query(User).filter(User.id == user_id)
            
            if not include_inactive:
                query = query.filter(User.is_active == True)
                
            user = query.first()
            
            if not user:
                return None
            
            return {
                "id": user.id,
                "username": user.username,
                "email": user.email,
                "role": user.role.value,
                "is_active": user.is_active,
                "created_at": user.created_at.isoformat() if user.created_at else None
            }
            
    except Exception as e:
        logger.error(f"Error getting user {user_id}: {e}")
        return None

def is_admin(user_id):
    """Check if user is admin with error handling"""
    try:
        user = get_user_by_id(user_id)
        return user and user.get('role') == 'admin'
    except:
        return False

# ============================================================================
# SESSION TOKEN MANAGEMENT (for future enhancement)
# ============================================================================

def generate_session_token(user_id):
    """Generate session token for user (placeholder for future enhancement)"""
    # Simple token for now - in production, use JWT or similar
    return hashlib.sha256(f"{user_id}:{secrets.token_hex(32)}:{time.time()}".encode()).hexdigest()

def validate_session_token(token, user_id):
    """Validate session token (placeholder for future enhancement)"""
    # Placeholder - implement proper token validation when needed
    return True

# ============================================================================
# INITIALIZATION WITH RETRY LOGIC
# ============================================================================

def create_default_admin(max_retries=3):
    """Create default admin with retry logic"""
    for attempt in range(max_retries):
        try:
            with get_auth_session() as session:
                # Check if admin exists
                admin_exists = session.query(User).filter(User.role == UserRole.ADMIN).first()
                if admin_exists:
                    return {"success": False, "message": "Admin already exists", "admin_exists": True}
                
                # Create admin
                result = create_user("admin", "admin@example.com", "admin123", "admin")
                return result
                
        except Exception as e:
            logger.warning(f"Admin creation attempt {attempt + 1} failed: {e}")
            if attempt < max_retries - 1:
                time.sleep(1)
            else:
                return {"success": False, "message": f"Failed after {max_retries} attempts: {str(e)}"}

# ============================================================================
# USER MANAGEMENT FUNCTIONS
# ============================================================================

def change_user_password(user_id, old_password, new_password):
    """Change user password with verification"""
    try:
        with get_auth_session() as session:
            user = session.query(User).filter(User.id == user_id).first()
            
            if not user:
                return {"success": False, "message": "User not found"}
            
            # Verify old password
            if not verify_password(old_password, user.password_hash):
                return {"success": False, "message": "Current password is incorrect"}
            
            # Update password
            user.password_hash = hash_password(new_password)
            
            logger.info(f"Password changed for user {user.username}")
            return {"success": True, "message": "Password updated successfully"}
            
    except Exception as e:
        logger.error(f"Error changing password for user {user_id}: {e}")
        return {"success": False, "message": "Failed to update password"}

def deactivate_user(user_id, admin_user_id):
    """Deactivate user account (admin only)"""
    try:
        if not is_admin(admin_user_id):
            return {"success": False, "message": "Admin privileges required"}
            
        with get_auth_session() as session:
            user = session.query(User).filter(User.id == user_id).first()
            
            if not user:
                return {"success": False, "message": "User not found"}
            
            if user.role == UserRole.ADMIN:
                return {"success": False, "message": "Cannot deactivate admin users"}
            
            user.is_active = False
            
            logger.info(f"User deactivated: {user.username} by admin {admin_user_id}")
            return {"success": True, "message": f"User '{user.username}' deactivated"}
            
    except Exception as e:
        logger.error(f"Error deactivating user {user_id}: {e}")
        return {"success": False, "message": "Failed to deactivate user"}

# ============================================================================
# STATISTICS AND MONITORING
# ============================================================================

def get_user_stats():
    """Get user statistics with error handling"""
    try:
        with get_auth_session() as session:
            total = session.query(User).count()
            active = session.query(User).filter(User.is_active == True).count()
            admins = session.query(User).filter(User.role == UserRole.ADMIN).count()
            
            return {
                "total_users": total,
                "active_users": active,
                "inactive_users": total - active,
                "admin_users": admins,
                "regular_users": active - admins
            }
    except Exception as e:
        logger.error(f"Error getting user stats: {e}")
        return {"total_users": 0, "active_users": 0, "inactive_users": 0, "admin_users": 0, "regular_users": 0}

# ============================================================================
# TESTING
# ============================================================================

if __name__ == "__main__":
    print("Testing enhanced authentication...")
    
    # Test database connection
    from database import test_connection
    if not test_connection():
        print("Database connection failed!")
        exit(1)
    
    # Test admin creation
    admin_result = create_default_admin()
    print(f"Admin creation: {admin_result}")
    
    # Test authentication
    if admin_result.get("success") or admin_result.get("admin_exists"):
        auth_result = authenticate_user("admin", "admin123")
        print(f"Authentication: {auth_result}")
    
    # Test stats
    stats = get_user_stats()
    print(f"Stats: {stats}")