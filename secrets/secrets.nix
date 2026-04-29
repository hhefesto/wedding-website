let
  admin = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBAolzCtF1t8rPKSRzvREQPBUjxRAi5medog8Ebi0n/G hhefesto@rdataa.com";
  xty = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ5cWilmgGZa24PrEFftyajabwxHvR4jxvkIvqkyCtmG root@nixos";

  users = [ admin ];
  systems = [ xty ];
in
{
  "wedding-db-password.age".publicKeys = users ++ systems;
  "wedding-backend-env.age".publicKeys = users ++ systems;
  "wedding-admin-password.age".publicKeys = users ++ systems;
  "wedding-admin-password-hash.age".publicKeys = users ++ systems;
}
