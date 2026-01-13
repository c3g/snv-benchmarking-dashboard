# ============================================================================
# file_manager.py
# ============================================================================
"""
File system operations for SNV Benchmarking Dashboard admin interface.

Main components:
- Directory listing and navigation
- File rename/delete operations  
- File upload to data folder
- Path security validation

*Requires admin privileges for all operations*
"""

import os
import shutil
import logging
from datetime import datetime
from config import DATA_FOLDER
from authorization import require_admin
import zipfile
import tempfile

logger = logging.getLogger(__name__)

# ============================================================================
# PATH SECURITY
# ============================================================================

def is_safe_path(path):
    """
    Validate path is within DATA_FOLDER to prevent directory traversal attacks.
    """
    if path is None:
        return True  # Will use DATA_FOLDER default
    
    try:
        # Resolve to absolute path and check it's within DATA_FOLDER
        real_path = os.path.realpath(path)
        real_data = os.path.realpath(DATA_FOLDER)
        return real_path.startswith(real_data)
    except Exception:
        return False

# ============================================================================
# FILE OPERATIONS
# ============================================================================

@require_admin
def list_directory(path=None, username=None, is_admin=False):
    """
    List contents of a directory within DATA_FOLDER.
    
    Args:
        path: Directory path (defaults to DATA_FOLDER)
        username: Username for logging
        is_admin: Admin status (required)
        
    Returns:
        dict: {success, files[], current_path} or {success, error}
    """
    if path is None:
        path = DATA_FOLDER
    
    if not is_safe_path(path):
        logger.warning(f"Path traversal attempt by {username}: {path}")
        return {'success': False, 'error': 'Invalid path'}
    
    try:
        items = []
        for name in sorted(os.listdir(path)):
            full_path = os.path.join(path, name)
            stat = os.stat(full_path)
            is_dir = os.path.isdir(full_path)
            
            items.append({
                'name': name,
                'path': full_path,
                'is_dir': is_dir,
                'size': stat.st_size if not is_dir else None,
                'modified': datetime.fromtimestamp(stat.st_mtime).strftime("%Y-%m-%d %H:%M")
            })
        
        logger.info(f"Directory listed by {username}: {path}")
        return {'success': True, 'files': items, 'current_path': path}
    
    except Exception as e:
        logger.error(f"list_directory failed: {e}")
        return {'success': False, 'error': str(e)}


@require_admin
def delete_file(path, username=None, is_admin=False):
    """
    Delete a file or empty directory within DATA_FOLDER.
    """
    if not is_safe_path(path):
        logger.warning(f"Path traversal attempt by {username}: {path}")
        return {'success': False, 'error': 'Invalid path'}
    
    if not os.path.exists(path):
        return {'success': False, 'error': 'File not found'}
    
    try:
        filename = os.path.basename(path)
        if os.path.isdir(path):
            os.rmdir(path)  # Only removes empty directories
        else:
            os.remove(path)
        
        logger.info(f"File deleted by {username}: {filename}")
        return {'success': True, 'message': f'Deleted {filename}'}
    except Exception as e:
        logger.error(f"delete_file failed: {e}")
        return {'success': False, 'error': str(e)}


@require_admin
def rename_file(path, new_name, username=None, is_admin=False):
    """
    Rename a file or directory within DATA_FOLDER.
    """
    if not is_safe_path(path):
        logger.warning(f"Path traversal attempt by {username}: {path}")
        return {'success': False, 'error': 'Invalid path'}
    
    if not os.path.exists(path):
        return {'success': False, 'error': 'File not found'}
    
    try:
        parent_dir = os.path.dirname(path)
        new_path = os.path.join(parent_dir, new_name)
        
        if not is_safe_path(new_path):
            return {'success': False, 'error': 'Invalid new name'}
        
        if os.path.exists(new_path):
            return {'success': False, 'error': 'A file with that name already exists'}
        
        old_name = os.path.basename(path)
        os.rename(path, new_path)
        
        logger.info(f"File renamed by {username}: {old_name} -> {new_name}")
        return {'success': True, 'message': f'Renamed to {new_name}', 'new_path': new_path}
    except Exception as e:
        logger.error(f"rename_file failed: {e}")
        return {'success': False, 'error': str(e)}


@require_admin
def upload_file(temp_path, filename, dest_folder=None, username=None, is_admin=False):
    """
    Upload a file to DATA_FOLDER (or specified subfolder).
    """
    if dest_folder is None:
        dest_folder = DATA_FOLDER
    
    if not is_safe_path(dest_folder):
        logger.warning(f"Path traversal attempt by {username}: {dest_folder}")
        return {'success': False, 'error': 'Invalid destination'}
    
    try:
        dest_path = os.path.join(dest_folder, filename)
        
        if os.path.exists(dest_path):
            return {'success': False, 'error': 'A file with that name already exists'}
        
        shutil.copy2(temp_path, dest_path)
        
        logger.info(f"File uploaded by {username}: {filename}")
        return {'success': True, 'message': f'Uploaded {filename}'}
    except Exception as e:
        logger.error(f"upload_file failed: {e}")
        return {'success': False, 'error': str(e)}
    


@require_admin
def create_zip_download(username=None, is_admin=False, max_size_mb=500):
    """
    Create a zip file of all data files for download.
    
    Args:
        username: Username for logging
        is_admin: Admin status (required)
        max_size_mb: Maximum total size allowed in MB
        
    Returns:
        dict: {success, zip_path} or {success, error}
    """
    try:
        max_size = max_size_mb * 1024 * 1024
        
        # Collect files and calculate size
        file_paths = []
        total_size = 0
        
        for name in os.listdir(DATA_FOLDER):
            full_path = os.path.join(DATA_FOLDER, name)
            if os.path.isfile(full_path):
                file_paths.append(full_path)
                total_size += os.path.getsize(full_path)
        
        if not file_paths:
            return {'success': False, 'error': 'No files to download'}
        
        if total_size > max_size:
            size_mb = round(total_size / (1024 * 1024))
            return {
                'success': False, 
                'error': f'Download too large ({size_mb} MB). Maximum: {max_size_mb} MB.'
            }
        
        # Create zip in temp directory
        zip_path = tempfile.mktemp(suffix='.zip')
        with zipfile.ZipFile(zip_path, 'w', zipfile.ZIP_DEFLATED) as zf:
            for file_path in file_paths:
                zf.write(file_path, os.path.basename(file_path))
        
        logger.info(f"Zip created by {username}: {len(file_paths)} files, {round(total_size/1024/1024, 1)} MB")
        return {'success': True, 'zip_path': zip_path, 'file_count': len(file_paths)}
    
    except Exception as e:
        logger.error(f"create_zip_download failed: {e}")
        return {'success': False, 'error': str(e)}