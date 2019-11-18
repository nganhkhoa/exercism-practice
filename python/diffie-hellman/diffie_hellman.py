import secrets

def private_key(p):
    s = secrets.randbelow(p)
    while s <= 1:
      s = secrets.randbelow(p)
    return s


def public_key(p, g, private):
    return pow(g, private, p)


def secret(p, public, private):
    return pow(public, private, p)
