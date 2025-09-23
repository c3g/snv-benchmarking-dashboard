# ============================================================================
# database.py
# ============================================================================
"""
Database connection, session management, and validation for SNV Benchmarking Dashboard.

Main components:
- SQLite database engine setup and configuration
- Session context management with proper error handling
- Database table creation and connection testing
- Environment validation and permission checks
- Enhanced with user table support
"""

import os
import logging
from config import DATABASE_PATH, DATA_FOLDER
from sqlalchemy import create_engine, text
from sqlalchemy.orm import sessionmaker
from contextlib import contextmanager
from models import Base  # Your existing public models

logger = logging.getLogger(__name__)

# ============================================================================
# SQLITE/POSTGRES CONFIGURATION
# ============================================================================
# Create data directory if it doesn't exist
data_dir = os.path.dirname(DATABASE_PATH)
if not os.path.exists(data_dir):
    os.makedirs(data_dir, exist_ok=True)

DATABASE_URL = f"sqlite:///{DATABASE_PATH}"
# to be switched to postgres later ------------------------------------------------------------

# Create the databse engine
engine = create_engine(DATABASE_URL)

# For creating a session
Session = sessionmaker(autocommit=False, autoflush=False, bind=engine)

# ============================================================================
# DATABASE MANAGEMENT FUNCTIONS
# ============================================================================

def create_tables():
    """Creates PUBLIC experiment tables from models.py"""
    Base.metadata.drop_all(bind=engine) #---- for dropping all the existing tables and metadata
    Base.metadata.create_all(bind=engine)

def create_user_tables():
    """Creates USER authentication and experiment tables"""
    try:
        from user_models import Base as UserBase
        UserBase.metadata.create_all(bind=engine)
        logger.info("User tables created successfully")
        return True
    except Exception as e:
        logger.error(f"Error creating user tables: {e}")
        return False

def create_all_tables():
    """Create both public and user tables"""
    try:
        # Create public tables
        create_tables()
        logger.info("Public tables created")
        
        # Create user tables
        create_user_tables()
        logger.info("User tables created")
        
        # Create default admin user
        create_initial_admin()
        
        return True
    except Exception as e:
        logger.error(f"Error creating all tables: {e}")
        return False

def create_initial_admin():
    """Create initial admin user if none exists"""
    try:
        with get_db_session() as session:
            from user_models import User, UserRole
            
            # Check if any admin exists
            admin_exists = session.query(User).filter(User.role == UserRole.ADMIN).first()
            
            if not admin_exists:
                from auth import create_user
                result = create_user("admin", "admin@admin.com", "admin123", "admin")
                
                if result["success"]:
                    logger.info("Created initial admin user: admin/admin123")
                else:
                    logger.error(f"Failed to create admin user: {result['message']}")
            else:
                logger.info("Admin user already exists")
                
    except Exception as e:
        logger.error(f"Error creating initial admin: {e}")

# Creates and returns a database session
@contextmanager
def get_db_session(): 
    session = Session()
    try:
        yield session
        session.commit()
    except Exception as e:
        session.rollback()
        logger.error(f"Database error: {e}")
        raise e
    finally:
        session.close()

# Get the SQLAlchemy engine
def get_engine():
    return engine

# Test connection to database
def test_connection():
    try:
        with get_db_session() as session: # Creates a database connection
            session.execute(text("SELECT 1"))
            logger.info("Database connection successful")
            return True
    except Exception as e:
        logger.error(f"Database connection failed: {e}")
        return False

# ============================================================================
# VALIDATION FUNCTIONS
# ============================================================================

def validate_environment():
    """
    Validate that the environment is properly set up for database operations.
    
    Checks to ensure the application can:
        - Read and write to the configured data folder
        - Create and access the database file
        - Handle file permissions correctly
        
        Returns:
            bool: True if all validation checks pass, False if any fail
        """
    from config import DATA_FOLDER
    
    # Check data folder access
    if not os.path.exists(DATA_FOLDER):
        logger.error(f"DATA_FOLDER does not exist: {DATA_FOLDER}")
        return False
    
    if not os.access(DATA_FOLDER, os.R_OK | os.W_OK):
        logger.error(f"Cannot read/write to DATA_FOLDER: {DATA_FOLDER}")
        return False
    
    # Check database directory access
    db_dir = os.path.dirname(DATABASE_PATH)
    if not os.path.exists(db_dir):
        logger.error(f"Database directory does not exist: {db_dir}")
        return False
        
    if not os.access(db_dir, os.R_OK | os.W_OK):
        logger.error(f"Cannot read/write to database directory: {db_dir}")
        return False
    
    logger.info("Environment validation successful")
    return True

# Drop all data and recreate empty structure
def drop_all_data():
    """Drop all PUBLIC tables and data, then recreate empty structure"""
    logger.info("Dropping all existing PUBLIC data...")
    Base.metadata.drop_all(bind=engine)
    logger.info("Recreating empty PUBLIC table structure...")
    Base.metadata.create_all(bind=engine)
    logger.info("All PUBLIC data dropped and tables recreated")

def drop_all_user_data():
    """Drop all USER tables and data (careful!)"""
    try:
        from user_models import Base as UserBase
        logger.warning("Dropping all USER data...")
        UserBase.metadata.drop_all(bind=engine)
        logger.info("All USER data dropped")
        return True
    except Exception as e:
        logger.error(f"Error dropping user data: {e}")
        return False

def reset_entire_database():
    """Reset both public and user data (DANGER!)"""
    logger.warning("RESETTING ENTIRE DATABASE - ALL DATA WILL BE LOST")
    try:
        # Drop all tables
        drop_all_user_data()
        drop_all_data()
        
        # Recreate all tables
        create_all_tables()
        
        logger.info("Database completely reset")
        return True
    except Exception as e:
        logger.error(f"Error resetting database: {e}")
        return False