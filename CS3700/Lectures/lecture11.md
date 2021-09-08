# Lecture 11

### NAT

- create a private network to utilize a single public IP address.
- modify packet headers with external IP, as well as change TCP / UDP ports
- load balancing to forward traffic
- acts as natural firewall
- stores public vs private IP connections in a table

### Protocols for Hole Punching

TURN (Traversal Using Relays around NAT)

- Use a separate relay to communicate between NATs

STUN

### DNS

- Map named addresses with IP addresses
- Named addresses are divided into zones; each zone has an admin
- Each DNS functions over a zone -- stores the hosts inside, and address of root servers
- Root servers know all top-level domains
- Glue Records contain address information  of name servers (to reduce lookups)

Queries

- type = A / AAAA (IPv4 / IPv6)
- name = domain name
- value = IP Address

Responses

- type = NS 
- name = domain name
- value = dns for this domain

CNAME

- type = CNAME
- name = hostname
- value = canonical hostname
- useful for aliasing / have multiple names point to the same thing

MX

- type = MX
- name = domain in email address
- value = name of mail server