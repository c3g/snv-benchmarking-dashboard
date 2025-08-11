# ============================================================================
# create_database.py
# ============================================================================
"""
Database initialization and population script for SNV Benchmarking Dashboard.

Main components:
- Environment validation before setup
- Database table creation from models
- CSV metadata population
- Connection testing and verification
"""

from database import drop_all_data, create_tables, test_connection, validate_environment
from models import *
from populate_metadata import *
import sys
import logging

logger = logging.getLogger(__name__)

def main():
    """Main function to create database and populate reference data"""
    logger.info("---- Creating Benchmarking Database ")
    
    # Validate environment first
    if not validate_environment():
        logger.error("Environment validation failed")
        sys.exit(1)
    
    # Test connection
    if not test_connection():
        logger.error("Cannot connect to database")
        sys.exit(1)

    # Code to Drop all existing data - commented out/ only run if needed
    # try:
    #    drop_all_data()
    #    logger.info("---- Dropped all existing data ----")
    # except Exception as e:
    #    logger.error(f"Failed to drop existing data: {e}")
    #    sys.exit(1)
    
    # Create tables
    try:
        create_tables()
        logger.info("---- Database tables created ----")
    except Exception as e:
        logger.error(f"Failed to create tables: {e}")
        sys.exit(1)
    
    # Add reference data
    if populate_database_from_csv():
        logger.info("---- Database setup completed ----")
    else:
        logger.error("Database setup failed")
        sys.exit(1)

if __name__ == "__main__":
    main()