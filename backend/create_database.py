from database import create_tables, test_connection
from models import *
from populate_metadata import *
import sys
import logging 

#logging
logger = logging.getLogger(__name__)

def main():
    """Main function to create database and populate reference data"""
    logging.info("Creating Benchmarking Database")
    
    # Test connection
    if not test_connection():
        logging.warning("Cannot connect to database")
        sys.exit(1)
    
    # Create tables
    create_tables() 
    
    # Add reference data
    populate_database_from_csv()

if __name__ == "__main__":
    main()
