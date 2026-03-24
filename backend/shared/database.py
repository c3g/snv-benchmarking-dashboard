# ============================================================================
# database.py
# ============================================================================
"""
Database connection and session management.

Components:
- SQLite engine setup 
- Session context manager with automatic commit/rollback
- Table creation and connection testing
"""

import os
import logging
from contextlib import contextmanager
from sqlalchemy import create_engine, text, event
from sqlalchemy.orm import sessionmaker
from config import DATABASE_PATH, SNV_DATA_FOLDER
from models import Base

logger = logging.getLogger(__name__)

# ============================================================================
# ENGINE SETUP
# ============================================================================

data_dir = os.path.dirname(DATABASE_PATH)
if not os.path.exists(data_dir):
    os.makedirs(data_dir, exist_ok=True)

DATABASE_URL = f"sqlite:///{DATABASE_PATH}"
engine = create_engine(DATABASE_URL)
Session = sessionmaker(autocommit=False, autoflush=False, bind=engine)


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
    """Session with automatic commit/rollback."""
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
    return engine

# ============================================================================
# INITIALIZATION
# ============================================================================

def migrate_database():
    """Add missing columns to existing tables. OPTIONAL"""
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
    """Create all tables defined in models.py if they don't exist."""
    Base.metadata.create_all(bind=engine)
    logger.info("All tables verified/created")


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
    """Validate read/write access to data folders and DB directory."""
    for folder in [SNV_DATA_FOLDER]:
        if not os.path.exists(folder):
            logger.error(f"Folder does not exist: {folder}")
            return False
        if not os.access(folder, os.R_OK | os.W_OK):
            logger.error(f"Cannot read/write: {folder}")
            return False

    db_dir = os.path.dirname(DATABASE_PATH)
    if not os.path.exists(db_dir):
        logger.error(f"Database directory does not exist: {db_dir}")
        return False
    if not os.access(db_dir, os.R_OK | os.W_OK):
        logger.error(f"Cannot read/write database directory: {db_dir}")
        return False

    logger.info("Environment validation successful")
    return True


def drop_all_data(confirm=False):
    """Drop all tables and recreate. Requires explicit confirm=True."""
    if not confirm:
        logger.error("drop_all_data() called without confirm=True — aborting")
        return False

    logger.warning("DROPPING ALL TABLES — THIS DELETES ALL DATA")
    Base.metadata.drop_all(bind=engine)
    Base.metadata.create_all(bind=engine)
    logger.info("All data dropped and tables recreated")
    return True