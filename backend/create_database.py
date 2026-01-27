# ============================================================================
# create_database.py
# ============================================================================
"""
Database initialization for SNV Benchmarking Dashboard.
Tests connection and validates environment - only creates tables if missing.
"""

import sys
import logging
from database import create_tables, test_connection, validate_environment

logger = logging.getLogger(__name__)

def main():
    """Initialize database - safe for repeated calls."""
    logger.info("---- Database Initialization ----")
    
    if not validate_environment():
        logger.error("Environment validation failed")
        sys.exit(1)
    
    if not test_connection():
        logger.error("Cannot connect to database")
        sys.exit(1)
    
    try:
        create_tables()
        logger.info("Database tables verified")
    except Exception as e:
        logger.error(f"Failed to verify tables: {e}")
        sys.exit(1)
    
    logger.info("---- Database ready ----")

if __name__ == "__main__":
    main()