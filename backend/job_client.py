# job_client.py
# ============================================================================
# Python client for Shiny to communicate with Flask/Celery backend
# ============================================================================

import requests
import time
import logging

logger = logging.getLogger(__name__)

# Flask server URL - change for production
FLASK_SERVER = "http://localhost:5000"

# ============================================================================
# API FUNCTIONS
# ============================================================================

def submit_file(filepath):
    """
    Submit a file to Flask for processing.
    
    Args:
        filepath: Local path to the file
        
    Returns:
        dict with job_id and status, or error
    """
    try:
        with open(filepath, 'rb') as f:
            files = {'file': f}
            response = requests.post(
                f"{FLASK_SERVER}/jobs/submit",
                files=files,
                timeout=120  # 2 min timeout for large files
            )
        
        if response.status_code == 200:
            return response.json()
        else:
            return {
                "success": False,
                "error": f"Server returned {response.status_code}: {response.text}"
            }
            
    except requests.exceptions.ConnectionError:
        return {
            "success": False,
            "error": "Cannot connect to processing server. Is Flask running?"
        }
    except Exception as e:
        return {
            "success": False,
            "error": str(e)
        }


def check_status(job_id):
    """
    Check the status of a submitted job.
    
    Args:
        job_id: The job ID from submit_file
        
    Returns:
        dict with job_id and status (PENDING, STARTED, SUCCESS, FAILURE)
    """
    try:
        response = requests.get(
            f"{FLASK_SERVER}/jobs/{job_id}/status",
            timeout=10
        )
        return response.json()
        
    except requests.exceptions.ConnectionError:
        return {
            "job_id": job_id,
            "status": "ERROR",
            "error": "Cannot connect to server"
        }
    except Exception as e:
        return {
            "job_id": job_id,
            "status": "ERROR",
            "error": str(e)
        }


def get_result(job_id):
    """
    Get the result of a completed job.
    
    Args:
        job_id: The job ID
        
    Returns:
        dict with status and csv_content (if successful)
    """
    try:
        response = requests.get(
            f"{FLASK_SERVER}/jobs/{job_id}/result",
            timeout=30
        )
        return response.json()
        
    except Exception as e:
        return {
            "status": "ERROR",
            "error": str(e)
        }


def submit_and_wait(filepath, poll_interval=2, max_wait=300):
    """
    Submit a file and wait for completion (blocking).
    Useful for testing.
    
    Args:
        filepath: Path to file
        poll_interval: Seconds between status checks
        max_wait: Maximum seconds to wait
        
    Returns:
        dict with final result or error
    """
    # Submit
    submit_result = submit_file(filepath)
    if "error" in submit_result:
        return submit_result
    
    job_id = submit_result.get("job_id")
    if not job_id:
        return {"success": False, "error": "No job_id returned"}
    
    # Poll
    elapsed = 0
    while elapsed < max_wait:
        status = check_status(job_id)
        
        if status.get("status") == "SUCCESS":
            return get_result(job_id)
        elif status.get("status") == "FAILURE":
            return {
                "success": False,
                "error": status.get("error", "Task failed")
            }
        
        time.sleep(poll_interval)
        elapsed += poll_interval
    
    return {
        "success": False,
        "error": f"Timeout after {max_wait} seconds"
    }


# ============================================================================
# TEST
# ============================================================================

if __name__ == "__main__":
    #  test
    import sys
    
    if len(sys.argv) > 1:
        filepath = sys.argv[1]
    else:
        filepath = "uploads/test.csv"
    
    print(f"Testing with: {filepath}")
    result = submit_and_wait(filepath)
    print(f"Result: {result}")