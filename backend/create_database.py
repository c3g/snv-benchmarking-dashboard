# ============================================================================
# create_database.py
# ============================================================================
"""
Database initialization and population script for SNV Benchmarking Dashboard.

Main components:
- Environment validation before setup
- Database table creation from models (public + user tables)
- CSV metadata population for public experiments
- Connection testing and verification
- Includes user authentication support
"""

from database import (
    drop_all_data, create_tables, create_user_tables, create_all_tables, 
    test_connection, validate_environment, create_initial_admin
)
from models import *
from populate_metadata import *
import sys
import logging

logger = logging.getLogger(__name__)

def create_public_database():
    """Create ONLY public experiment tables and populate with CSV data"""
    logger.info("---- Creating PUBLIC Benchmarking Database ----")
    
    # Validate environment first
    if not validate_environment():
        logger.error("Environment validation failed")
        return False
    
    # Test connection
    if not test_connection():
        logger.error("Cannot connect to database")
        return False

    # Create public tables only
    try:
        create_tables()
        logger.info("---- Public database tables created ----")
    except Exception as e:
        logger.error(f"Failed to create public tables: {e}")
        return False
    
    # Add reference data from CSV
    if populate_database_from_csv():
        logger.info("---- Public database setup completed ----")
        return True
    else:
        logger.error("Public database setup failed")
        return False

def create_user_database():
    """Create ONLY user authentication and experiment tables"""
    logger.info("---- Creating USER Authentication Database ----")
    
    # Validate environment first
    if not validate_environment():
        logger.error("Environment validation failed")
        return False
    
    # Test connection
    if not test_connection():
        logger.error("Cannot connect to database")
        return False

    # Create user tables
    try:
        if create_user_tables():
            logger.info("---- User database tables created ----")
            
            # Create initial admin user
            create_initial_admin()
            logger.info("---- User database setup completed ----")
            return True
        else:
            logger.error("Failed to create user tables")
            return False
    except Exception as e:
        logger.error(f"Failed to create user tables: {e}")
        return False

def create_complete_database():
    """Create BOTH public and user databases"""
    logger.info("---- Creating COMPLETE Benchmarking Database ----")
    
    # Validate environment first
    if not validate_environment():
        logger.error("Environment validation failed")
        return False
    
    # Test connection
    if not test_connection():
        logger.error("Cannot connect to database")
        return False

    # Option to drop existing data (commented out for safety)
    # try:
    #     from database import reset_entire_database
    #     reset_entire_database()
    #     logger.info("---- Dropped all existing data ----")
    # except Exception as e:
    #     logger.error(f"Failed to drop existing data: {e}")
    #     return False
    
    # Create all tables (public + user)
    try:
        if create_all_tables():
            logger.info("---- All database tables created ----")
        else:
            logger.error("Failed to create all tables")
            return False
    except Exception as e:
        logger.error(f"Failed to create tables: {e}")
        return False
    
    # Add reference data from CSV (for public experiments)
    if populate_database_from_csv():
        logger.info("---- Complete database setup completed ----")
        return True
    else:
        logger.error("Database setup failed")
        return False

def main():
    """Main function with options for different setup types"""
    import argparse
    
    parser = argparse.ArgumentParser(description='Create SNV Benchmarking Database')
    parser.add_argument('--type', 
                       choices=['public', 'user', 'complete'], 
                       default='complete',
                       help='Type of database to create')
    
    args = parser.parse_args()
    
    if args.type == 'public':
        success = create_public_database()
    elif args.type == 'user':
        success = create_user_database()
    else:  # complete
        success = create_complete_database()
    
    if not success:
        logger.error("Database creation failed")
        sys.exit(1)
    
    logger.info("Database creation successful!")

if __name__ == "__main__":
    main()