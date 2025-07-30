# database manager (connects to the database, creates sessions tests connection)
import os
from config import DATABASE_PATH
from sqlalchemy import create_engine, text
from sqlalchemy.orm import sessionmaker
from contextlib import contextmanager
from models import Base

# ============================================================================
# SQLITE/POSTGRES CONFIGURATION
# ============================================================================
# Create data directory if it doesn't exist
data_dir = os.path.dirname(DATABASE_PATH)
if not os.path.exists(data_dir):
    os.makedirs(data_dir, exist_ok=True)

DATABASE_URL = f"sqlite:///{DATABASE_PATH}"
# to be switched to postgres later ------------------------------------------------------------

# Create the databse engine
engine = create_engine(DATABASE_URL)

# For creating a session
Session = sessionmaker(autocommit=False, autoflush=False, bind=engine)


# ============================================================================
# DATABSE MANAGEMENT FUNCTIONS
# ============================================================================

# Creates tables from models.py
def create_tables():
    Base.metadata.drop_all(bind=engine) #---- for dropping all the existing tables and metadata
    Base.metadata.create_all(bind=engine)

# Creates and returns a database session
@contextmanager
def get_db_session(): 
    session = Session()
    try:
        yield session
        session.commit()
    except Exception as e:
        session.rollback()
        print(f"Database error: {e}")  # Add logging
        raise e
    finally:
        session.close()


# Get the SQLAlchemy engine
def get_engine():
    return engine

# Test connection to database
def test_connection():
    try:
        with get_db_session() as session: # Creates a database connection
            session.execute(text("SELECT 1"))
            print("Connection Successful")
            return True
    except Exception as e:
        print(f"Database connection failed: {e}")
        return False