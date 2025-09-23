# ============================================================================
# auth.py
# ============================================================================
"""
Authentication system for SNV Benchmarking Dashboard.
Handles user registration, authentication, session management, and admin functions.
"""

import hashlib
import secrets
import logging
import time
from datetime import datetime, timedelta
from contextlib import contextmanager
from typing import Optional, Dict, Any

from sqlalchemy.exc import IntegrityError, OperationalError
from sqlalchemy.orm import sessionmaker

from user_models import User, UserRole
from database import get_db_session, engine

logger = logging.getLogger(__name__)

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
    for attempt in range(max_retries):
        try:
            session = sessionmaker(bind=engine)()
            yield session
            session.commit()
            break
        except OperationalError as e:
            session.rollback()
            if attempt == max_retries - 1:
                logger.error(f"Database connection failed after {max_retries} attempts: {e}")
                raise
            time.sleep(0.5 * (attempt + 1))  # Exponential backoff
        except Exception as e:
            session.rollback()
            logger.error(f"Database error in auth session: {e}")
            raise
        finally:
            try:
                session.close()
            except:
                pass

# ============================================================================
# PASSWORD MANAGEMENT
# ============================================================================

def hash_password(password: str) -> str:
    """Hash password using SHA256 + salt"""
    salt = secrets.token_hex(16)
    password_hash = hashlib.sha256((password + salt).encode()).hexdigest()
    return f"{salt}:{password_hash}"

def verify_password(password: str, stored_hash: str) -> bool:
    """Verify password against stored hash"""
    try:
        salt, password_hash = stored_hash.split(':')
        test_hash = hashlib.sha256((password + salt).encode()).hexdigest()
        return test_hash == password_hash
    except:
        return False

# ============================================================================
# USER MANAGEMENT FUNCTIONS
# ============================================================================

def create_user(username: str, email: str, password: str, role: str = 'user') -> Dict[str, Any]:
    """
    Create new user account with comprehensive validation and error handling.
    
    Args:
        username: Unique username (3-50 characters)
        email: Valid email address
        password: Password (minimum 6 characters)
        role: 'user' or 'admin'
    
    Returns:
        Dict with success status, message, and user data if successful
    """
    try:
        # Input validation
        if not username or len(username.strip()) < 3:
            return {"success": False, "message": "Username must be at least 3 characters", "code": "INVALID_USERNAME"}
        
        if not email or '@' not in email:
            return {"success": False, "message": "Valid email address required", "code": "INVALID_EMAIL"}
        
        if not password or len(password) < 6:
            return {"success": False, "message": "Password must be at least 6 characters", "code": "WEAK_PASSWORD"}
        
        if role not in ['user', 'admin']:
            return {"success": False, "message": "Invalid role specified", "code": "INVALID_ROLE"}
        
        user_role = UserRole.ADMIN if role == 'admin' else UserRole.USER
        
        with get_auth_session() as session:
            # Check for existing username or email
            existing = session.query(User).filter(
                (User.username == username.strip()) | (User.email == email.strip().lower())
            ).first()
            
            if existing:
                if existing.username == username.strip():
                    return {"success": False, "message": "Username already exists", "code": "USERNAME_EXISTS"}
                else:
                    return {"success": False, "message": "Email already registered", "code": "EMAIL_EXISTS"}
            
            # Create new user
            new_user = User(
                username=username.strip(),
                email=email.strip().lower(),
                role=user_role,
                is_active=True,
                created_at=datetime.now()
            )
            new_user.set_password(password)
            
            session.add(new_user)
            session.flush()  # Get user ID
            
            user_data = {
                "id": new_user.id,
                "username": new_user.username,
                "email": new_user.email,
                "role": new_user.role.value,
                "created_at": new_user.created_at.isoformat()
            }
            
            logger.info(f"User created successfully: {new_user.username} (ID: {new_user.id})")
            return {
                "success": True,
                "message": f"User '{username}' created successfully",
                "user": user_data
            }
            
    except IntegrityError as e:
        logger.error(f"Error creating user {username}: {e}")
        return {"success": False, "message": "Username or email already exists", "code": "INTEGRITY_ERROR"}
    except Exception as e:
        logger.error(f"Error creating user {username}: {e}")
        return {"success": False, "message": f"Database error: {str(e)}", "code": "DATABASE_ERROR"}

def authenticate_user(username: str, password: str) -> Dict[str, Any]:
    """
    Authenticate user credentials with comprehensive error handling.
    
    Args:
        username: Username or email address
        password: User password
    
    Returns:
        Dict with success status, user data if successful, error details if failed
    """
    try:
        with get_auth_session() as session:
            # Find user by username or email
            user = session.query(User).filter(
                (User.username == username.strip()) | (User.email == username.strip().lower()),
                User.is_active == True
            ).first()
            
            if not user:
                logger.warning(f"Authentication failed: User '{username}' not found")
                return {"success": False, "message": "Invalid credentials", "code": "USER_NOT_FOUND"}
            
            # Verify password
            if not user.check_password(password):
                logger.warning(f"Authentication failed: Invalid password for user '{username}'")
                return {"success": False, "message": "Invalid credentials", "code": "INVALID_PASSWORD"}
            
            # Update last login
            user.last_login = datetime.now()
            
            # Authentication successful
            user_data = {
                "id": user.id,
                "username": user.username,
                "email": user.email,
                "role": user.role.value,
                "is_admin": user.role == UserRole.ADMIN,
                "last_login": user.last_login.isoformat() if user.last_login else None,
                "created_at": user.created_at.isoformat() if user.created_at else None
            }
            
            logger.info(f"User authenticated: {user.username} (ID: {user.id})")
            return {
                "success": True,
                "user": user_data,
                "session_token": generate_session_token(user.id)
            }
            
    except Exception as e:
        logger.error(f"Authentication error for '{username}': {e}")
        return {"success": False, "message": "Authentication system error", "code": "SYSTEM_ERROR"}

def get_user_by_id(user_id: int, include_inactive: bool = False) -> Optional[Dict[str, Any]]:
    """Get user by ID with error handling"""
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
                "is_admin": user.role == UserRole.ADMIN,
                "last_login": user.last_login.isoformat() if user.last_login else None,
                "created_at": user.created_at.isoformat() if user.created_at else None
            }
            
    except Exception as e:
        logger.error(f"Error getting user {user_id}: {e}")
        return None

def verify_user_credentials(username: str, password: str) -> Dict[str, Any]:
    """
    Credential verification for R shinymanager integration.
    Returns format expected by shinymanager check_credentials function.
    """
    auth_result = authenticate_user(username, password)
    
    if auth_result["success"]:
        # Return format expected by shinymanager
        user_data = auth_result["user"]
        return {
            "result": True,
            "user_info": {
                "username": user_data["username"],
                "user": user_data["username"],  # shinymanager expects 'user' field
                "user_id": user_data["id"],
                "admin": user_data["is_admin"],
                "role": user_data["role"],
                "email": user_data["email"]
            }
        }
    else:
        return {"result": False, "message": auth_result.get("message", "Authentication failed")}

# ============================================================================
# ADMIN FUNCTIONS
# ============================================================================

def list_all_users() -> Dict[str, Any]:
    """Get list of all users (admin only)"""
    try:
        with get_auth_session() as session:
            users = session.query(User).order_by(User.created_at.desc()).all() #get all users 
            
            user_list = []
            for user in users:
                user_list.append({
                    "id": user.id,
                    "username": user.username,
                    "email": user.email,
                    "role": user.role.value,
                    "is_active": user.is_active,
                    "last_login": user.last_login.isoformat() if user.last_login else None,
                    "created_at": user.created_at.isoformat() if user.created_at else None
                })
            
            return {"success": True, "users": user_list}
            
    except Exception as e:
        logger.error(f"Error listing users: {e}")
        return {"success": False, "message": str(e)}

def update_user_role(admin_user_id: int, target_user_id: int, new_role: str) -> Dict[str, Any]: #-------------------------------------------
    """Update user role (admin only)"""
    try:
        # Verify admin privileges
        admin_user = get_user_by_id(admin_user_id)
        if not admin_user or not admin_user.get("is_admin"):
            return {"success": False, "message": "Admin privileges required"}
        
        if new_role not in ['user', 'admin']:
            return {"success": False, "message": "Invalid role specified"}
        
        with get_auth_session() as session:
            target_user = session.query(User).filter(User.id == target_user_id).first()
            
            if not target_user:
                return {"success": False, "message": "User not found"}
            
            old_role = target_user.role.value
            target_user.role = UserRole.ADMIN if new_role == 'admin' else UserRole.USER
            
            logger.info(f"Admin {admin_user_id} changed user {target_user_id} role from {old_role} to {new_role}")
            return {"success": True, "message": f"User role updated to {new_role}"}
            
    except Exception as e:
        logger.error(f"Error updating user role: {e}")
        return {"success": False, "message": "Failed to update user role"}

def deactivate_user(admin_user_id: int, target_user_id: int) -> Dict[str, Any]:
    """Deactivate user account (admin only)"""
    try:
        # Verify admin privileges
        admin_user = get_user_by_id(admin_user_id)
        if not admin_user or not admin_user.get("is_admin"):
            return {"success": False, "message": "Admin privileges required"}
        
        if admin_user_id == target_user_id:
            return {"success": False, "message": "Cannot deactivate your own account"}
        
        with get_auth_session() as session:
            target_user = session.query(User).filter(User.id == target_user_id).first()
            
            if not target_user:
                return {"success": False, "message": "User not found"}
            
            target_user.is_active = False
            
            logger.info(f"Admin {admin_user_id} deactivated user {target_user_id}")
            return {"success": True, "message": f"User '{target_user.username}' deactivated"}
            
    except Exception as e:
        logger.error(f"Error deactivating user: {e}")
        return {"success": False, "message": "Failed to deactivate user"}

# ============================================================================
# INITIALIZATION FUNCTIONS
# ============================================================================

def create_default_admin(username: str = "snv_admin", password: str = "snv_admin123") -> Dict[str, Any]:
    """Create default admin user if none exists"""
    try:
        with get_auth_session() as session:
            # Check if any admin exists
            admin_exists = session.query(User).filter(User.role == UserRole.ADMIN).first()
            
            if admin_exists:
                logger.info("Admin user already exists")
                return {"success": True, "message": "Admin user already exists"}
            
            # Create default admin
            result = create_user(username, f"{username}@admin.local", password, "admin")
            
            if result["success"]:
                logger.info(f"Created default admin user: {username}")
                return {"success": True, "message": f"Default admin created: {username}"}
            else:
                logger.error(f"Failed to create default admin: {result['message']}")
                return result
                
    except Exception as e:
        logger.error(f"Error creating default admin: {e}")
        return {"success": False, "message": str(e)}

def create_user_tables():
    """Create all user-related database tables"""
    try:
        from user_models import Base
        Base.metadata.create_all(bind=engine)
        logger.info("User tables created successfully")
        
        # Create default admin
        create_default_admin()
        
        return {"success": True, "message": "User tables initialized"}
        
    except Exception as e:
        logger.error(f"Error creating user tables: {e}")
        return {"success": False, "message": str(e)}

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

def is_admin(user_id: int) -> bool:
    """Check if user has admin privileges"""
    try:
        user = get_user_by_id(user_id)
        return user and user.get('is_admin', False)
    except:
        return False

def get_user_stats() -> Dict[str, Any]:
    """Get user statistics for admin dashboard"""
    try:
        with get_auth_session() as session:
            total_users = session.query(User).count()
            active_users = session.query(User).filter(User.is_active == True).count()
            admin_users = session.query(User).filter(User.role == UserRole.ADMIN).count()
            
            # Recent registrations (last 30 days)
            thirty_days_ago = datetime.now() - timedelta(days=30)
            recent_registrations = session.query(User).filter(
                User.created_at >= thirty_days_ago
            ).count()
            
            return {
                "success": True,
                "stats": {
                    "total_users": total_users,
                    "active_users": active_users,
                    "admin_users": admin_users,
                    "recent_registrations": recent_registrations
                }
            }
            
    except Exception as e:
        logger.error(f"Error getting user stats: {e}")
        return {"success": False, "message": str(e)}
    
# ============================================================================
# SESSION TOKEN MANAGEMENT - for future
# ============================================================================

def generate_session_token(user_id: int) -> str:
    """Generate session token for user"""
    return hashlib.sha256(f"{user_id}:{secrets.token_hex(32)}:{time.time()}".encode()).hexdigest()

def validate_session_token(token: str, user_id: int) -> bool:
    """Validate session token (placeholder for future enhancement)"""
    # Basic validation - can be enhanced with token storage/expiration
    return bool(token and len(token) == 64)
