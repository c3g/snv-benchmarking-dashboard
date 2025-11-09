# authorization.py - Authorization in backend

import logging
from functools import wraps

logger = logging.getLogger(__name__)

def require_admin(func):
    """
    Decorator to enforce admin permission.
    Checks is_admin flag from frontend (determined by COManage groups).
    """
    @wraps(func)
    def wrapper(*args, **kwargs):
        is_admin = kwargs.get('is_admin', False)  # Check is_admin state received  from frontend
        username = kwargs.get('username', 'unknown')
        
        if not is_admin:
            error_msg = f"Unauthorized: {username} lacks admin privileges"
            logger.warning(error_msg)
            return {'success': False, 'error': error_msg, 'unauthorized': True}
        
        logger.info(f"Admin permission verified for {username}")
        return func(*args, **kwargs)
    return wrapper