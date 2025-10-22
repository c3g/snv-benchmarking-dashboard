# authorization.py - Authorization in backend

import logging
from functools import wraps

logger = logging.getLogger(__name__)

def check_admin_permission(username):
    """
    Check if username has admin privileges (for now: hardcoded)
    """
    ADMIN_USERNAMES = ['snv_admin'] 
    
    # admin check DEBUG
    print("\n" + "="*60)
    print("ADMIN CHECK")
    print("="*60)
    print(f"Received username: '{username}'")
    
    if not username:
        print("RESULT: FALSE - No username provided")
        print("="*60 + "\n")
        logger.warning("Authorization check with no username")
        return False
    
    username_clean = str(username).strip().lower()
    print(f"Cleaned username: '{username_clean}'")
    
    is_admin = username_clean in ADMIN_USERNAMES
    print(f"ADMIN RESULT: {is_admin}")
    print("="*60 + "\n")
    
    logger.info(f"Admin check for {username}: {is_admin}")
    return is_admin

def require_admin(func):
    """Decorator to enforce admin permission on Python functions"""
    @wraps(func)
    def wrapper(*args, **kwargs):
        username = kwargs.get('username')
        
        if not check_admin_permission(username):
            error_msg = f"Unauthorized: {username or 'anonymous'} lacks admin privileges"
            print(f"AUTHORIZATION FAILED: {error_msg}\n")
            logger.warning(error_msg)
            return {
                'success': False,
                'error': error_msg,
                'unauthorized': True
            }
        
        print(f"AUTHORIZATION PASSED for {username}\n")
        return func(*args, **kwargs)
    
    return wrapper