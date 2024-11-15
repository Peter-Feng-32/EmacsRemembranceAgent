import socket

def start_server():
    host = '127.0.0.1'  # Localhost
    port = 9998        # Port to listen on

    server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_socket.bind((host, port))
    server_socket.listen(1)
    print(f"Server listening on {host}:{port}")

    numresponses = 0

    conn, addr = server_socket.accept()
    print(f"Connection established with {addr}")
    while True:


        # Receive data
        recvdstr = conn.recv(1024).decode('utf-8')

        if recvdstr:
            index, name, query = recvdstr.split("|")
            print(f"Received from client: {index.strip()} | {name.strip()} | {query.strip()}")

            # Send a response
            response = f"{index.strip()} | {name.strip()} | Hello from the server {numresponses}!\n"
            numresponses += 1
            conn.sendall(response.encode('utf-8'))

    conn.close()
    print("Connection closed.")

if __name__ == "__main__":
    start_server()