# ============================================================================
# user_management.py
# ============================================================================
"""
User management for SNV Benchmarking Dashboard.

Main components:
- User creation/retrieval on OIDC login
- User lookup by ID or username
- Admin status management
"""

from datetime import datetime
from database import get_db_session
from models import User
import logging

logger = logging.getLogger(__name__)

# ============================================================================
# USER SYNC ON LOGIN
# ============================================================================

def get_or_create_user(username, email, full_name, is_admin):
    """
    Get existing user or create new one. Called on every login.
    Updates last_login and syncs admin status from OIDC groups.
    
    Args:
        username: OIDC username (eppn or sub)
        email: User email from OIDC
        full_name: Display name from OIDC
        is_admin: Admin status from OIDC group membership
    
    Returns:
        dict: {"user_id": int, "username": str, "is_admin": bool, "is_new": bool}
    """
    try:
        with get_db_session() as session:
            # Try to find existing user by username first, then by email
            user = session.query(User).filter_by(username=username).first()
            
            if not user and email:
                user = session.query(User).filter_by(email=email).first()
                if user:
                    # Update username if found by email (migration case)
                    user.username = username
                    logger.info(f"Updated username for existing user: {email} -> {username}")
            
            if user:
                # Update existing user on each login
                user.last_login = datetime.now()
                user.is_admin = is_admin
                user.email = email
                user.full_name = full_name
                is_new = False
                logger.info(f"User login: {username} (ID: {user.id}, admin: {is_admin})")
            else:
                # Create new user
                user = User(
                    username=username,
                    email=email,
                    full_name=full_name,
                    is_admin=is_admin,
                    created_at=datetime.now(),
                    last_login=datetime.now()
                )
                session.add(user)
                is_new = True
                logger.info(f"New user created: {username} (admin: {is_admin})")
            
            session.flush()  # Ensure ID is generated
            
            return {
                "user_id": user.id,
                "username": user.username,
                "is_admin": user.is_admin,
                "is_new": is_new,
                "success": True
            }
            
    except Exception as e:
        logger.error(f"Error in get_or_create_user: {e}")
        return {
            "user_id": None,
            "username": username,
            "is_admin": False,
            "is_new": False,
            "success": False,
            "error": str(e)
        }

# ============================================================================
# USER LOOKUP FUNCTIONS
# ============================================================================

def get_user_by_id(user_id):
    """
    Get user by database ID.
    
    Args:
        user_id: User's database ID
        
    Returns:
        dict: User info or None if not found
    """
    if not user_id:
        return None
        
    try:
        with get_db_session() as session:
            user = session.query(User).filter_by(id=user_id).first()
            
            if user:
                return {
                    "user_id": user.id,
                    "username": user.username,
                    "email": user.email,
                    "full_name": user.full_name,
                    "is_admin": user.is_admin,
                    "created_at": user.created_at.isoformat() if user.created_at else None,
                    "last_login": user.last_login.isoformat() if user.last_login else None
                }
            return None
            
    except Exception as e:
        logger.error(f"Error getting user by ID {user_id}: {e}")
        return None

def get_user_by_username(username):
    """
    Get user by username.
    
    Args:
        username: User's OIDC username
        
    Returns:
        dict: User info or None if not found
    """
    if not username:
        return None
        
    try:
        with get_db_session() as session:
            user = session.query(User).filter_by(username=username).first()
            
            if user:
                return {
                    "user_id": user.id,
                    "username": user.username,
                    "email": user.email,
                    "full_name": user.full_name,
                    "is_admin": user.is_admin
                }
            return None
            
    except Exception as e:
        logger.error(f"Error getting user by username {username}: {e}")
        return None

# ============================================================================
# USER STATISTICS (for admin panel later)
# ============================================================================

def get_all_users():
    """
    Get all users (admin function).
    
    Returns:
        list: List of user dicts
    """
    try:
        with get_db_session() as session:
            users = session.query(User).order_by(User.created_at.desc()).all()
            
            return [{
                "user_id": u.id,
                "username": u.username,
                "email": u.email,
                "full_name": u.full_name,
                "is_admin": u.is_admin,
                "created_at": u.created_at.isoformat() if u.created_at else None,
                "last_login": u.last_login.isoformat() if u.last_login else None
            } for u in users]
            
    except Exception as e:
        logger.error(f"Error getting all users: {e}")
        return []

def get_user_experiment_count(user_id):
    """
    Get count of experiments owned by user.
    
    Args:
        user_id: User's database ID
        
    Returns:
        int: Number of experiments
    """
    from models import Experiment
    
    if not user_id:
        return 0
        
    try:
        with get_db_session() as session:
            count = session.query(Experiment).filter_by(owner_id=user_id).count()
            return count
            
    except Exception as e:
        logger.error(f"Error getting experiment count for user {user_id}: {e}")
        return 0