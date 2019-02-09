utilities
hashPassword: password
^ (PBKDF2 derivedKeySHA1Password: password salt: MelanchatConfiguration salt) hex