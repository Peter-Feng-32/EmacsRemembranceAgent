import socket

def send_message_to_server():
    host = '127.0.0.1'  # Server address
    port = 9999         # Server port

    # Create a socket
    client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    client_socket.connect((host, port))

    print("Connected to the server.")

    # Send a message
    message = "Hello from the client!\n\n"
    client_socket.sendall(message.encode('utf-8'))

    # Close the connection
    client_socket.close()
    print("Message sent and connection closed.")

if __name__ == "__main__":
    send_message_to_server()