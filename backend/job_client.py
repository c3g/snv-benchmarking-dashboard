# ============================================================================
# job_client.py — Shiny-side job submission client - INTEGRATED INTO APP
# ============================================================================
"""
Used by the app to:
  1. Upload input file to S3
  2. Submit job to Flask with S3 path + metadata
  3. Poll job status
  4. Retrieve completed result

Configuration: set FLASK_SERVER and BUCKET_NAME before deployment.
"""

import boto3
import requests
import os
import logging
from datetime import datetime

logger = logging.getLogger(__name__)

# deployment environment - TO BE CONFIGURED
FLASK_SERVER = "http://localhost:5000"
BUCKET_NAME = "test-bucket"

# set endpoint url for S3 client
endpoint_url="https://objets.juno.calculquebec.ca" 

# S3 prefix for uploaded input files
S3_INPUT_PREFIX = "vcf_inputs"


# ============================================================================
# S3 UPLOAD
# ============================================================================

def upload_to_s3(local_path, object_key):
    """Upload input file to S3 bucket, return full S3 URI."""

    s3 = boto3.client('s3', endpoint_url=endpoint_url)
    
    s3.upload_file(local_path, BUCKET_NAME, object_key)
    bucket_path = f"s3://{BUCKET_NAME}/{object_key}"
    logger.info(f"Uploaded {local_path} to {bucket_path}")
    return bucket_path


# ============================================================================
# JOB SUBMISSION
# ============================================================================

def submit_file(filepath, metadata=None):
    """
    Upload input file to S3, then submit job to Flask.

    Args:
        filepath: Local path to input file
        metadata: Dict with experiment and ownership fields

    Returns:
        dict with job_id and status, or error key on failure
    """
    try:
        filename   = os.path.basename(filepath)
        timestamp  = datetime.now().strftime("%Y%m%d_%H%M%S")
        object_key = f"{S3_INPUT_PREFIX}/{timestamp}_{filename}"

        bucket_path = upload_to_s3(filepath, object_key)

        # pass original filename so tasks.py can use it for the result key
        meta = metadata or {}
        meta['original_filename'] = os.path.splitext(filename)[0]

        response = requests.post(
            f"{FLASK_SERVER}/jobs/submit",
            json={'bucket_path': bucket_path, 'metadata': meta},
            timeout=30
        )

        if response.status_code == 200:
            return response.json()
        else:
            return {"error": f"Server returned {response.status_code}: {response.text}"}

    except Exception as e:
        logger.error(f"submit_file failed: {e}")
        return {"error": str(e)}


# ============================================================================
# STATUS AND RESULT
# ============================================================================

def check_status(job_id):
    """
    Poll job status from Flask.

    Returns:
        dict with status (PENDING / STARTED / SUCCESS / FAILURE) and job_id
    """
    try:
        response = requests.get(
            f"{FLASK_SERVER}/jobs/{job_id}/status",
            timeout=10
        )
        return response.json()
    except Exception as e:
        logger.error(f"check_status failed for {job_id}: {e}")
        return {"job_id": job_id, "status": "ERROR", "error": str(e)}


def get_result(job_id):
    """
    Get result S3 key from Flask, then download the result file from S3.

    Returns:
        dict with status, result_content, and metadata
    """
    try:
        response = requests.get(
            f"{FLASK_SERVER}/jobs/{job_id}/result",
            timeout=30
        )
        data = response.json()

        if data.get('status') != 'SUCCESS':
            return data

        # Download result CSV from S3 to a local temp file
        result_s3_key = data['result_s3_key']
        local_result  = f"/tmp/{job_id}_result.csv"
        s3 = boto3.client('s3', endpoint_url=endpoint_url)
        s3.download_file(BUCKET_NAME, result_s3_key, local_result)

        logger.info(f"Downloaded result from s3://{BUCKET_NAME}/{result_s3_key}")
        return {
            'status': 'SUCCESS',
            'result_local_path': local_result,
            'metadata': data.get('metadata', {})
        }

    except Exception as e:
        logger.error(f"get_result failed for {job_id}: {e}")
        return {"status": "ERROR", "error": str(e)}