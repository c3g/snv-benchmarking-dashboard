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
"""

import os
import logging
from config import DATABASE_PATH, DATA_FOLDER
from sqlalchemy import create_engine, text
from sqlalchemy.orm import sessionmaker
from contextlib import contextmanager
from models import Base

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
# DATABSE MANAGEMENT FUNCTIONS
# ============================================================================

# Creates tables from models.py
def create_tables():
    Base.metadata.drop_all(bind=engine) #---- for dropping all the existing tables and metadata
    Base.metadata.create_all(bind=engine)

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