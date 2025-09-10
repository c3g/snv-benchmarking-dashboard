# ============================================================================
# database.py
# ============================================================================
"""
Database connection, session management, and validation for SNV Benchmarking Dashboard.

Main components:
- PostgreSQL/SQLite database engine setup and configuration  
- Session context management with proper error handling
- Database table creation with auto-created ENUMs
- Connection testing and environment validation
"""

import os
import logging
from dotenv import load_dotenv
from config import DATABASE_PATH, DATA_FOLDER
from sqlalchemy import create_engine, text
from sqlalchemy.orm import sessionmaker
from contextlib import contextmanager
from models import Base

# Load environment variables from .env file
load_dotenv()

logger = logging.getLogger(__name__)

# Global variables for engine and session
engine = None
Session = None
IS_POSTGRESQL = False

def initialize_database_connection():
    """Initialize or reinitialize database connection"""
    global engine, Session, IS_POSTGRESQL
    
    # Database environment variables
    DB_HOST = os.getenv('DB_HOST')
    DB_PORT = os.getenv('DB_PORT', '5432')
    DB_NAME = os.getenv('DB_NAME')
    DB_USER = os.getenv('DB_USER')
    DB_PASSWORD = os.getenv('DB_PASSWORD')

    #  Database based on environment variables
    if all([DB_HOST, DB_NAME, DB_USER, DB_PASSWORD]):
        # PostgreSQL configuration
        DATABASE_URL = f"postgresql://{DB_USER}:{DB_PASSWORD}@{DB_HOST}:{DB_PORT}/{DB_NAME}"
        try:
            # Dispose of old engine if it exists
            if engine:
                engine.dispose()
                
            engine = create_engine(
                DATABASE_URL,
                pool_size=10,
                max_overflow=20,
                pool_pre_ping=True,
                echo=False
            )
            IS_POSTGRESQL = True
            logger.info("Using PostgreSQL database")
        except Exception as e:
            logger.error(f"Failed to connect to PostgreSQL: {e}")
            logger.info("Falling back to SQLite")
            # Fall back to SQLite
            data_dir = os.path.dirname(DATABASE_PATH)
            if not os.path.exists(data_dir):
                os.makedirs(data_dir, exist_ok=True)
            DATABASE_URL = f"sqlite:///{DATABASE_PATH}"
            if engine:
                engine.dispose()
            engine = create_engine(DATABASE_URL)
            IS_POSTGRESQL = False
    else:
        # SQLite fallback configuration
        logger.warning("PostgreSQL environment variables not found, using SQLite fallback")
        
        data_dir = os.path.dirname(DATABASE_PATH)
        if not os.path.exists(data_dir):
            os.makedirs(data_dir, exist_ok=True)
        DATABASE_URL = f"sqlite:///{DATABASE_PATH}"
        if engine:
            engine.dispose()
        engine = create_engine(DATABASE_URL)
        IS_POSTGRESQL = False

    # Create new session factory
    Session = sessionmaker(autocommit=False, autoflush=False, bind=engine)
    logger.info("Database connection initialized")

# Initialize on import
initialize_database_connection()

@contextmanager
def get_db_session(): 
    """Get database session with automatic reconnection"""
    global Session
    
    # Try to create session, reinitialize if needed
    session = None
    try:
        session = Session()
        yield session
        session.commit()
    except Exception as e:
        if session:
            session.rollback()
        
        # Check if it's a connection issue
        if "connection" in str(e).lower() or "database" in str(e).lower():
            logger.warning("Database connection issue detected, reinitializing...")
            initialize_database_connection()
            # Try again with new connection
            session = Session()
            try:
                yield session
                session.commit()
            except Exception as e2:
                session.rollback()
                logger.error(f"Database error after reconnection: {e2}")
                raise e2
            finally:
                session.close()
        else:
            logger.error(f"Database error: {e}")
            raise e
    finally:
        if session:
            session.close()

def force_reconnect():
    """Force reconnection to database - after major database operations"""
    logger.info("Forcing database reconnection...")
    initialize_database_connection()

# ============================================================================
# TABLE CREATION
# ============================================================================

def create_tables():
    """Create tables with auto-created ENUMs - much simpler!"""
    
    #Base.metadata.drop_all(bind=engine) ------- to drop all tables before recreating them, to be used if needed
    Base.metadata.create_all(bind=engine)
    
    logger.info(f"Database tables created successfully ({'PostgreSQL' if IS_POSTGRESQL else 'SQLite'})")

def get_engine():
    """Get the SQLAlchemy engine"""
    return engine

def test_connection():
    """Test database connection"""
    try:
        with get_db_session() as session:
            session.execute(text("SELECT 1"))
            logger.info("Database connection successful")
            return True
    except Exception as e:
        logger.error(f"Database connection failed: {e}")
        return False

def validate_environment():
    """Validate that the environment is properly set up for database operations."""
    
    # Check data folder access
    if not os.path.exists(DATA_FOLDER):
        logger.error(f"DATA_FOLDER does not exist: {DATA_FOLDER}")
        return False
    
    if not os.access(DATA_FOLDER, os.R_OK | os.W_OK):
        logger.error(f"Cannot read/write to DATA_FOLDER: {DATA_FOLDER}")
        return False

    if IS_POSTGRESQL:
        logger.info("Environment validation successful (PostgreSQL)")
        return True
    
    # (For SQLite) check database directory access
    db_dir = os.path.dirname(DATABASE_PATH)
    if not os.path.exists(db_dir):
        logger.error(f"Database directory does not exist: {db_dir}")
        return False
        
    if not os.access(db_dir, os.R_OK | os.W_OK):
        logger.error(f"Cannot read/write to database directory: {db_dir}")
        return False
    
    logger.info("Environment validation successful (SQLite)")
    return True

def drop_all_data():
    """Drop all tables and data, then recreate empty structure"""
    logger.info("Dropping all existing data...")
    
    Base.metadata.drop_all(bind=engine)
    logger.info("Recreating empty table structure...")
    Base.metadata.create_all(bind=engine)
    
    logger.info("All data dropped and tables recreated")

def clear_all_data():
    """Clear all data (still keeping tables)""" # ------ This function is not used for deleting a record from the db 
    with get_db_session() as session:
        # Disable foreign key checks temporarily
        if IS_POSTGRESQL:
            session.execute(text("SET session_replication_role = replica"))
            
            # Delete all data from all tables
            for table in reversed(Base.metadata.sorted_tables):
                session.execute(text(f"TRUNCATE TABLE {table.name} CASCADE"))
                
            session.execute(text("SET session_replication_role = DEFAULT"))
        else:
            # SQLite
            session.execute(text("PRAGMA foreign_keys = OFF"))
            
            for table in reversed(Base.metadata.sorted_tables):
                session.execute(text(f"DELETE FROM {table.name}"))
                
            session.execute(text("PRAGMA foreign_keys = ON"))