# ============================================================================
# database.py
# ============================================================================
"""
Database connection and session management for SNV Benchmarking Dashboard.

Main components:
- SQLite database engine setup and configuration
- Session context management with automatic commit/rollback
- Connection testing and environment validation
"""

import os
import logging
from contextlib import contextmanager
from sqlalchemy import create_engine, text, event
from sqlalchemy.orm import sessionmaker
from config import DATABASE_PATH, DATA_FOLDER
from models import Base

logger = logging.getLogger(__name__)

# ============================================================================
# DATABASE CONFIGURATION
# ============================================================================

data_dir = os.path.dirname(DATABASE_PATH)
if not os.path.exists(data_dir):
    os.makedirs(data_dir, exist_ok=True)

DATABASE_URL = f"sqlite:///{DATABASE_PATH}"

engine = create_engine(DATABASE_URL)
Session = sessionmaker(autocommit=False, autoflush=False, bind=engine)

# SQLite safety settings (must be AFTER engine creation)
@event.listens_for(engine, "connect")
def set_sqlite_pragma(dbapi_connection, connection_record):
    cursor = dbapi_connection.cursor()
    cursor.execute("PRAGMA journal_mode=WAL")
    cursor.execute("PRAGMA foreign_keys=ON")
    cursor.close()

# ============================================================================
# SESSION MANAGEMENT
# ============================================================================

@contextmanager
def get_db_session(): 
    """Create database session with automatic commit/rollback."""
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

def get_engine():
    """Return the SQLAlchemy engine."""
    return engine

# ============================================================================
# INITIALIZATION & TESTING
# ============================================================================

def migrate_database():
    """Add missing columns to existing tables."""
    migrations = [
        # experiments table - new columns for public/private functionality
        ("experiments", "is_public", "BOOLEAN DEFAULT 1"),
        ("experiments", "created_by_username", "VARCHAR(100)"),
        ("experiments", "owner_id", "INTEGER REFERENCES users(id)"),
    ]
    
    with engine.connect() as conn:
        for table, column, col_type in migrations:
            try:
                conn.execute(text(f"SELECT {column} FROM {table} LIMIT 1"))
            except Exception:
                try:
                    conn.execute(text(f"ALTER TABLE {table} ADD COLUMN {column} {col_type}"))
                    conn.commit()
                    logger.info(f"Added column {column} to {table}")
                except Exception as e:
                    logger.warning(f"Could not add {column} to {table}: {e}")


def create_tables():
    """Create tables if they don't exist, then run migrations."""
    Base.metadata.create_all(bind=engine)
    migrate_database()

def test_connection():
    """Test database connectivity."""
    try:
        with get_db_session() as session:
            session.execute(text("SELECT 1"))
            logger.info("Database connection successful")
            return True
    except Exception as e:
        logger.error(f"Database connection failed: {e}")
        return False

def validate_environment():
    """Validate read/write access to data folder and database directory."""
    
    if not os.path.exists(DATA_FOLDER):
        logger.error(f"DATA_FOLDER does not exist: {DATA_FOLDER}")
        return False
    
    if not os.access(DATA_FOLDER, os.R_OK | os.W_OK):
        logger.error(f"Cannot read/write to DATA_FOLDER: {DATA_FOLDER}")
        return False
    
    db_dir = os.path.dirname(DATABASE_PATH)
    if not os.path.exists(db_dir):
        logger.error(f"Database directory does not exist: {db_dir}")
        return False
        
    if not os.access(db_dir, os.R_OK | os.W_OK):
        logger.error(f"Cannot read/write to database directory: {db_dir}")
        return False
    
    logger.info("Environment validation successful")
    return True

def drop_all_data(confirm=False):
    """Drop all tables and recreate empty structure. Requires explicit confirmation."""
    if not confirm:
        logger.error("drop_all_data() called without confirm=True - aborting")
        return False
    
    logger.warning("DROPPING ALL TABLES - THIS DELETES ALL DATA")
    Base.metadata.drop_all(bind=engine)
    Base.metadata.create_all(bind=engine)
    logger.info("All data dropped and tables recreated")
    return True