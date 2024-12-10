import socket
import threading

def start_server():
    host = '127.0.0.1'  # Localhost
    port = 9998        # Port to listen on

    server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_socket.bind((host, port))
    server_socket.listen(5)
    print(f"Server listening on {host}:{port}")
    num_connections = 0

    while True:
        client_socket, client_address = server_socket.accept()
        client_thread = threading.Thread(target=handle_client, args=(client_socket, client_address, num_connections))
        client_thread.start()
        num_connections += 1

    conn.close()
    print("Connection closed.")

def handle_client(client_socket, client_address, num_connections):
    """Handles a single client connection."""
    print(f"New connection from {client_address}")
    try:
        while True:
            data = client_socket.recv(1024)
            if not data:
                break
            file_path = "/Users/Peter/Desktop/EmacsRA/documents/athena.txt"
            print(data)
            response = "{\"similarity_score\": " + str(num_connections) + ", \"document_title\": \"Test Title\"" + ", \"file_path\": \"" + file_path + "\"" + "}"
            print(response)
            client_socket.sendall(response.encode('utf-8'))  # Echo back the data
    finally:
        print(f"Connection closed for {client_address}")
        client_socket.close()

if __name__ == "__main__":
    start_server()