"""Local preview of the website in web/ before it goes to Vercel. Like
`python -m http.server`, but every reload revalidates, so you always see the latest
build.

    python deploy/serve.py [port]        (default 8241), then open http://127.0.0.1:8241/
"""
import functools
import http.server
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
PORT = int(sys.argv[1]) if len(sys.argv) > 1 else 8241
ROOT = os.path.join(HERE, "..", "web")


class NoCache(http.server.SimpleHTTPRequestHandler):
    extensions_map = {
        **http.server.SimpleHTTPRequestHandler.extensions_map,
        ".js": "text/javascript", ".mjs": "text/javascript", ".css": "text/css",
        ".wasm": "application/wasm", ".json": "application/json", ".svg": "image/svg+xml",
    }

    def end_headers(self):
        self.send_header("Cache-Control", "no-cache")
        super().end_headers()


if __name__ == "__main__":
    handler = functools.partial(NoCache, directory=os.path.abspath(ROOT))
    with http.server.ThreadingHTTPServer(("127.0.0.1", PORT), handler) as httpd:
        print(f"Serving {os.path.abspath(ROOT)} at http://127.0.0.1:{PORT}/", flush=True)
        httpd.serve_forever()
