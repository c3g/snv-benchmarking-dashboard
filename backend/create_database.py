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
from config import METADATA_CSV_PATH
import os

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
    # Optionally populate from CSV if it exists (for initial migration only)

    if os.path.exists(METADATA_CSV_PATH):
        logger.info("Found existing CSV metadata, importing...")
        if populate_database_from_csv():
            logger.info("---- Database populated from CSV ----")
        else:
            logger.warning("CSV import failed, but database tables created")
    else:
        logger.info("---- Database initialized (empty) ----")
        logger.info("Upload experiments via web interface to populate")

    logger.info("---- Database setup completed ----")

if __name__ == "__main__":
    main()