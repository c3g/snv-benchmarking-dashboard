# ============================================================================
# create_database.py
# ============================================================================
"""
Database initialization for SNV Benchmarking Dashboard.
Safe for repeated calls - validates environment and ensures DB is ready.
"""

import sys
import os
import logging
from database import create_tables, test_connection, validate_environment, get_engine
from config import DATABASE_PATH

logger = logging.getLogger(__name__)


def check_existing_database():
    """Check if database already exists and has data."""
    if not os.path.exists(DATABASE_PATH):
        return {"exists": False, "has_data": False}
    
    try:
        from database import get_db_session
        from models import Experiment
        
        with get_db_session() as session:
            count = session.query(Experiment).count()
            return {"exists": True, "has_data": count > 0, "experiment_count": count}
    except Exception as e:
        logger.warning(f"Could not check existing data: {e}")
        return {"exists": True, "has_data": False, "error": str(e)}


def main():
    """Initialize database"""
    logger.info("=" * 50)
    logger.info("Database Initialization")
    logger.info("=" * 50)
    
    # Step 1: Validate environment
    if not validate_environment():
        logger.error("Environment validation failed")
        sys.exit(1)
    logger.info("Environment validated")
    
    # Step 2: Check existing database
    db_status = check_existing_database()
    
    if db_status["exists"]:
        if db_status.get("has_data"):
            logger.info(f"Existing database found with {db_status.get('experiment_count', '?')} experiments")
        else:
            logger.info("Existing database found (empty or new)")
    else:
        logger.info("No existing database - will create new")
    
    # Step 3: Test connection
    if not test_connection():
        logger.error("Cannot connect to database")
        sys.exit(1)
    logger.info("Database connection successful")
    
    # Step 4: Create/verify tables
    try:
        create_tables()
        logger.info("Database tables verified")
    except Exception as e:
        logger.error(f"Failed to verify tables: {e}")
        sys.exit(1)
    

if __name__ == "__main__":
    main()