fun gcd (0, n) = n
  | gcd (m, n) = gcd(n mod m, m)()