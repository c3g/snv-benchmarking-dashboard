from database import create_tables, get_db_session, test_connection
from models import *
from populate_metadata import *
import sys

def main():
    """Main function to create database and populate reference data"""
    print("Creating Benchmarking Database\n")
    
    # Test connection
    if not test_connection():
        print("Cannot connect to database.")
        sys.exit(1)
    
    print("🗑️ Dropping all existing tables and data...")
    create_tables()  # This empties everything
    print("✅ Fresh tables created")
    
    # Add reference data
    if populate_database_from_csv():
        print("completed")

    else:
        print("Database setup failed")
        sys.exit(1)

if __name__ == "__main__":
    main()
