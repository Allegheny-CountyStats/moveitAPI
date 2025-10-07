import requests
import pandas as pd
import tempfile
import os
import json


def auth_move_it(base_url, payload):
    # Build URL
    url = f"https://moveit.{base_url}/api/v1/token"
    # Post Auth
    response = requests.post(url, data=payload, headers={'Content-Type': 'application/json'})
    try:
        response.raise_for_status()
    except requests.exceptions.HTTPError as e:
        return "Error: " + str(e)
    # Get tokens
    tokens = response.json()
    # Return auth token list
    return tokens


def read_move_it_file(base_url, tokens, file_id, file_type="csv", sheet_download=None):
    # Create Temp file for download
    tmp = tempfile.NamedTemporaryFile(delete=False)
    token = tokens['access_token']
    # Build file URL
    url = f"https://moveit.{base_url}/api/v1/files/{file_id}/download"
    # Request
    if file_type == "excel":
        c_type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    else:
        c_type = "text/csv"

    headers = {'Authorization': f'Bearer {token}'}
    response = requests.get(url, headers=headers, stream=True)

    with open(tmp.name, 'wb') as f:
        f.write(response.content)

    # Read tmp file by file type
    if file_type == "csv":
        data = pd.read_csv(tmp.name, low_memory=False)
    elif file_type == "txt":
        data = pd.read_csv(tmp.name, sep='\t', low_memory=False)
    elif file_type == "excel":
        data = pd.read_excel(tmp.name, sheet_name=sheet_download)

    # Delete Temp File
    os.remove(tmp.name)
    # Return data to user
    return data


def available_files(base_url, tokens):
    # Read access Token
    token = f"Bearer {tokens['access_token']}"
    # Build URL
    url = f"https://moveit.{base_url}/api/v1/files"
    # Send Get request
    response = requests.get(url, headers={"Authorization": token})
    # Files list
    files = response.json()

    # Files dataframe
    items = files['items']
    # Page Number & total pages for while loop
    page = int(files['paging']['page'])
    total_pages = int(files['paging']['totalPages'])

    # Iterate over additional pages
    while page != total_pages:
        # Next Page number
        next_page = page + 1
        # Build URL
        url_page = f"{url}?page={next_page}"

        # Get page of files
        response = requests.get(url_page, headers={"Authorization": token})

        # Files list
        files = response.json()

        # Bind to previous pages
        items.extend(files['items'])

        # Get current page
        page = int(files['paging']['page'])
        # Check total_pages
        total_pages = int(files['paging']['totalPages'])

    return items


def available_folders(base_url, tokens):
    # Load Auth token
    token = f"Bearer {tokens['access_token']}"
    # Build URL
    url = f"https://moveit.{base_url}/api/v1/folders"

    # Get request for folders
    response = requests.get(url, headers={'Authorization': token})

    # List of folders
    folders = response.json()

    # Folder details as list of dictionaries
    items = folders['items']

    # Page Number & total pages for while loop
    page = int(folders['paging']['page'])
    total_pages = int(folders['paging']['totalPages'])

    # Iterate over additional pages
    while page != total_pages:
        # Next Page number
        next_page = page + 1
        # Build URL
        url_page = f"{url}?page={next_page}"

        # Get page of files
        response = requests.get(url_page, headers={'Authorization': token})
        folders = response.json()

        # Bind to previous pages
        items.extend(folders['items'])
        # Get current page and total pages
        page = folders['paging']['page']
        total_pages = folders['paging']['totalPages']

    return items


def upload_move_it_file(base_url, tokens, folder_id, file_path, file_type, chunked=False):
    # Check dependency
    try:
        import requests
    except ImportError:
        raise ImportError("Package 'requests' needed for this function to work. Please install it.")

    # Fix Chunked on Linux
    if chunked is None:
        chunked = False

    # Load Auth token
    token = f"Bearer {tokens['access_token']}"

    # Build URL
    url = f"https://moveit.{base_url}/api/v1/folders/{folder_id}/files"

    size = os.path.getsize(file_path)

    headers = {
        "Authorization": token,
        "accept": "application/json",
        "Content-Type": "multipart/form-data"
    }

    if size >= 40000000 or chunked:
        headers["Transfer-Encoding"] = "chunked"

    with open(file_path, 'rb') as f:
        files = {'file': (os.path.basename(file_path), f, file_type)}
        response = requests.post(url, headers=headers, files=files)

    if response.status_code not in [201, 200]:
        raise Exception(response.status_code)


