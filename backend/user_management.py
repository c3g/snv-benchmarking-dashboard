from datetime import datetime
from database import get_db_session
from models import User
import logging

logger = logging.getLogger(__name__)

def get_or_create_user(username, email, full_name, is_admin):
    """
    Get existing user or create new one.
    Updates last_login and admin status on each login.
    
    Returns:
        dict: {"user_id": int, "username": str, "is_new": bool}
    """
    with get_db_session() as session:
        user = session.query(User).filter_by(username=username).first()
        
        if user:
            # Update existing user
            user.last_login = datetime.now()
            user.is_admin = is_admin
            user.email = email
            user.full_name = full_name
            is_new = False
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
        
        session.flush()
        
        return {
            "user_id": user.id,
            "username": user.username,
            "is_new": is_new
        }