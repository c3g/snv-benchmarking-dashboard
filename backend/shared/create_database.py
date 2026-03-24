# ============================================================================
# create_database.py
# ============================================================================
"""
Database initialization for VarBench Dashboard.
only validates environment and ensures DB is ready.
"""
import sys
import os
import logging
from database import create_tables, test_connection, validate_environment, get_engine
from config import DATABASE_PATH

logger = logging.getLogger(__name__)


def check_existing_database():
    """Check if database exists and has data in either snv or sv tables."""
    if not os.path.exists(DATABASE_PATH):
        return {"exists": False, "has_data": False}

    try:
        from sqlalchemy import text
        engine = get_engine()
        with engine.connect() as conn:
            snv_count = conn.execute(text("SELECT COUNT(*) FROM snv_experiments")).scalar()
            sv_count = conn.execute(text("SELECT COUNT(*) FROM sv_experiments")).scalar()
            total = snv_count + sv_count
            return {
                "exists": True,
                "has_data": total > 0,
                "snv_count": snv_count,
                "sv_count": sv_count
            }
    except Exception as e:
        logger.warning(f"Could not check existing data: {e}")
        return {"exists": True, "has_data": False, "error": str(e)}


def main():
    """Initialize and check database"""
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
            logger.info(
                f"Existing database found — "
                f"{db_status.get('snv_count', '?')} SNV, "
                f"{db_status.get('sv_count', '?')} SV experiments"
            )
        else:
            logger.info("Existing database found (empty or new tables)")
    else:
        logger.info("No existing database — will create new")

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