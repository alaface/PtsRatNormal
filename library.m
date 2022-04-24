// Bin
// INPUT = a pair of integers
// OUTPUT = the modified binomial a choose b

Bin := function(a,b)
 if a lt 0 then return 0; end if;
 return Binomial(a,b);
end function;

// del
// INPUT = a subset of indices I, an integer t > 0
// OUTPUT = the dimension of the variety \delta_{I,\sigma_t}

del := function(I,t)
 return #I+2*t-1;
end function;

// kc
// INPUT = two integers, n,d, a sequence m
// OUTPUT = the number k_C

kc := function(n,d,m)
 return Ceiling((&+m-n*d)/(#m-n-2));
end function;

// ka
// INPUT = two integers, n,d, a sequence m
// a subset of indices I, an integer t > 0
// OUTPUT = the number k_{I,\sigma_t}

ka := function(n,d,m,I,t)
 if #I eq 0 then 
  return t*kc(n,d,m)-(#I+t-1)*d;
 else 
  return &+[m[i] : i in I]+t*kc(n,d,m)-(#I+t-1)*d;
 end if;
end function;

// Fun
// INPUT = five integers, a,r,e,n,t
// OUTPUT = the value of F_t(a,r,e,n)

Fu := function(a,r,e,n,t)
 if t eq 0 then return Bin(a,n); end if;
  cf := Bin(a,n) + &+[Bin(r-n-4+i,i)*Bin(a+i,n) : i in [1..t]];
  su := &+[Bin(e,i)*$$(a,r,e,n-i,t-i) : i in [1..t]];
  return cf-su;
end function;

// Dim
// INPUT = two integers, n,d, a sequence m
// OUTPUT = the dimension of the linear system
// L_{n,d}(m_1,...,m_r)

Dim := function(n,d,m)
 r := #m;
 k := kc(n,d,m);
 e := k*(r-n-2)-(&+m-n*d);
 dim := 0;
 for t in [0..Floor(n/2)] do
  if t*k le (t-1)*(d-1) then break; end if;
  for I in &join[Subsets({1..r},k) : k in [0..n-2*t]] do
   a := n+ka(n,d,m,I,t)-del(I,t)-1;
   dim := dim + (-1)^#I*Fu(a,r,e,n,t);
  end for;
 end for;
 return dim;
end function;
