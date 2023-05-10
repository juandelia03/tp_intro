type Usuario = (Integer, String)
type Relacion = (Usuario, Usuario)
type Publicacion = (Usuario, String, [Usuario])
type RedSocial = ([Usuario], [Relacion], [Publicacion])

longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = longitud xs + 1

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Auxiliares
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece t [] = False
pertenece t (x:xs) | t == x = True 
                   | otherwise = pertenece t xs

--

mismosElementosAux :: (Eq t) => [t] -> [t] -> Bool
mismosElementosAux [] ys = True
mismosElementosAux (x:xs) ys | pertenece x ys = mismosElementosAux xs ys
                             | otherwise = False

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos xs ys = (mismosElementosAux xs ys) && (mismosElementosAux ys xs)


redSocialValida :: RedSocial -> Bool
redSocialValida rs = (usuariosValidos (usuarios rs)) && 
                     (relacionesValidas (usuarios rs) (relaciones rs)) && 
                     (publicacionesValidas (usuarios rs) (publicaciones rs))

--Auxiliares para hacer usuariosValidos

usuarioValido :: Usuario -> Bool
usuarioValido u = ((idDeUsuario u) > 0) && (longitud (nombreDeUsuario u) > 0)

usuarioIds :: [Usuario] -> [Integer]
usuarioIds [] = []
usuarioIds (u:us) = fst u : (usuarioIds us)

todosDistintos  :: (Eq t) => [t] -> Bool
todosDistintos x = todosDistintosAux x [] 

todosDistintosAux  :: (Eq t) => [t] -> [t] -> Bool
todosDistintosAux  [] y = True
todosDistintosAux  (x:xs) y | pertenece (x) y = False
                       | otherwise = todosDistintosAux xs (x:y)

noHayIdsRepetidos :: [Usuario] -> Bool
noHayIdsRepetidos [] = True
noHayIdsRepetidos us = todosDistintos(usuarioIds us)

-- hay que chequear que todos los usuarios de un lista de usuarios tengan id mayor a 0 y un nombre de longitud mayor a 0 
usuariosConFormatoValido :: [Usuario] -> Bool
usuariosConFormatoValido [] = True
usuariosConFormatoValido (u:us) | usuarioValido u = usuariosConFormatoValido us
                                | otherwise = False

usuariosValidos :: [Usuario] -> Bool
usuariosValidos us = noHayIdsRepetidos us && usuariosConFormatoValido us

--Aca terminan las auxiliares de usuariosValidos

--Auxiliares para hacer relacionesValidas

reversoAux :: [t] -> [t] -> [t]
reversoAux [] y = y
reversoAux (x:xs) y = reversoAux xs (x:y)

reverso :: [t] -> [t]
reverso x = reversoAux x []

noHayRelacionesRepetidas :: [Relacion] -> Bool
noHayRelacionesRepetidas [] = True
noHayRelacionesRepetidas (r:rs) | pertenece (head ids) (tail ids) = False
                                | otherwise = noHayRelacionesRepetidas rs
                                 where ids = listaDeIdsDeRelaciones (r:rs)

listaDeIdsDeRelaciones :: [Relacion] -> [[Integer]]
listaDeIdsDeRelaciones [] = []
listaDeIdsDeRelaciones (r:rs) = [idDeUsuario(fst r), idDeUsuario(snd r)] : listaDeIdsDeRelaciones rs

relacionesAsimetricas :: [Relacion] -> Bool
relacionesAsimetricas [] = True
relacionesAsimetricas (r:rs) | not (pertenece (reverso (head(ids))) (tail(ids))) = relacionesAsimetricas rs
                             | otherwise = False
                            where ids = listaDeIdsDeRelaciones (r:rs)

usuariosDeRelacionValidos :: [Usuario] -> [Relacion] -> Bool
usuariosDeRelacionValidos us [] = True
usuariosDeRelacionValidos us (r:rs) | ((pertenece (fst (r)) us) && (pertenece (snd (r)) us)) &&
                                         ((fst (r)) /= (snd (r))) = usuariosDeRelacionValidos us rs
                                    | otherwise = False

relacionesValidas :: [Usuario] -> [Relacion] -> Bool
relacionesValidas us rs = (usuariosDeRelacionValidos us rs) && (relacionesAsimetricas rs) && (noHayRelacionesRepetidas rs)

-- Aca terminan las auxiliares de relacionesValidas

-- Auxiliares para hacer PublicacionesValidas

publicacionesSinMeGusta :: [Publicacion] -> [Publicacion]
publicacionesSinMeGusta [] = []
publicacionesSinMeGusta ((a,b,c):ps) = (a,b,[]) : publicacionesSinMeGusta ps

noHayPublicacionesRepetidas:: [Publicacion] -> Bool
noHayPublicacionesRepetidas [] = True
noHayPublicacionesRepetidas (p:ps) | not (pertenece x xs) = noHayPublicacionesRepetidas xs
                                   | otherwise = False
                                   where (x:xs) = publicacionesSinMeGusta(p:ps)

usuariosDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDePublicacionSonUsuariosDeRed us [] = True
usuariosDePublicacionSonUsuariosDeRed us ((p1,p2,p3):ps) | pertenece p1 us = usuariosDePublicacionSonUsuariosDeRed us ps
                                                         | otherwise = False

publicacionesValidas :: [Usuario] -> [Publicacion] -> Bool
publicacionesValidas us ps = (usuariosDePublicacionSonUsuariosDeRed us ps) && (noHayPublicacionesRepetidas ps)

-- Aca terminan las auxiliares de PublicacionesValidas

-- Aca empieza cadena de amigos

relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool
relacionadosDirecto u1 u2 (us,rs,ps) = pertenece (u1,u2) rs || pertenece (u2,u1) rs

cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
-- que pasa cuando le damos un unico usuario o ninguno; Segun la especificacion deberia ser undefined pero creemos que True
-- cadenaDeAmigos [] rs = True
-- cadenaDeAmigos [_] rs = True

cadenaDeAmigos (u1:u2:us) rs | longitud (u1:u2:us) == 2 = relacionadosDirecto u1 u2 rs
                             | relacionadosDirecto u1 u2 rs == True = cadenaDeAmigos (u2:us) rs
                             | otherwise = False



sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed rs [] = True
sonDeLaRed rs (u:us) | pertenece u (usuarios rs) = sonDeLaRed rs us
                     | otherwise = False

empiezaCon :: (Eq t) => t -> [t] -> Bool
empiezaCon x [] = False
empiezaCon x xs = head xs == x

terminaCon :: (Eq t) => t -> [t] -> Bool
terminaCon x [] = False
terminaCon x xs = last xs == x

sinRepetidos :: (Eq t) => [t] -> Bool
sinRepetidos xs = todosDistintos xs

-- aca terminan los predicados auxiliares obligatorios

-- auxiliares propias

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos x = eliminarRepetidosAux x []

eliminarRepetidosAux :: (Eq t) => [t] -> [t] -> [t]
eliminarRepetidosAux [] ys = reverso ys
eliminarRepetidosAux (x:xs) ys | pertenece x ys = eliminarRepetidosAux xs ys
                               | otherwise = eliminarRepetidosAux xs (x:ys)

--Ejercicios obligatorios

--Ejercicio 1 
proyectarNombres :: [Usuario] -> [[Char]]
proyectarNombres [] = []
proyectarNombres (u:us) = eliminarRepetidos((nombreDeUsuario u) : proyectarNombres us)

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios rs = proyectarNombres (usuarios rs)

--Ejercicio 2
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (us,[],ps) u = []
amigosDe (us,(r:rs),ps) u | fst r == u = snd r : amigosDe (us,rs,ps) u
                          | snd r == u = fst r : amigosDe (us,rs,ps) u
                          | otherwise = amigosDe (us,rs,ps) u

--Ejercicio 3
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos rs u = fromInteger(longitud((amigosDe rs u)))
-- usamos fromInteger porque longitud esta definido para Integer y no Int

--Ejercicio 4
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos ([u1],rs,ps) = u1
usuarioConMasAmigos ((u:us),rs,ps) | (cantidadDeAmigos ((u:us),rs,ps) u) >= (cantidadDeAmigos ((u:us),rs,ps) (usuarioConMasAmigos (us,rs,ps))) = u
                                   | otherwise = usuarioConMasAmigos (us,rs,ps)

--Ejercicio 5
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([],rs,ps) = False
estaRobertoCarlos ((u:us),rs,ps) | cantidadDeAmigos ((u:us),rs,ps) u > 1000000 = True
                                 | otherwise = estaRobertoCarlos (us,rs,ps)

--Ejercicio 6
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (us,rs,[]) u = []
publicacionesDe (us,rs,(p:ps)) u | usuarioDePublicacion p == u = p : (publicacionesDe (us,rs,ps) u)
                                 | otherwise = publicacionesDe (us,rs,ps) u

--Ejercicio 7 
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (us,rs,[]) u = []
publicacionesQueLeGustanA (us,rs,(p:ps)) u | pertenece u (likesDePublicacion p) = p : (publicacionesQueLeGustanA (us,rs,ps) u) 
                                           | otherwise = (publicacionesQueLeGustanA (us,rs,ps) u) 

--Ejercicio 8
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones rs u1 u2 = mismosElementos (publicacionesQueLeGustanA rs u1) (publicacionesQueLeGustanA rs u2)

--Ejercicio 9 
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel ([],rs,ps) u1 = False
tieneUnSeguidorFiel (u:us,rs,ps) u1 | u == u1 = tieneUnSeguidorFiel (us,rs,ps) u1
                                    | contenidoPrimeraEnSegunda (publicacionesDe (u:us,rs,ps) u1) (publicacionesQueLeGustanA (u:us,rs,ps) u) = True
                                    | otherwise = tieneUnSeguidorFiel (us,rs,ps) u1

contenidoPrimeraEnSegunda :: (Eq t) => [t] -> [t] -> Bool
contenidoPrimeraEnSegunda [] ys = True
contenidoPrimeraEnSegunda (x:xs) ys | pertenece x ys = contenidoPrimeraEnSegunda xs ys
                                    | otherwise = False

--Ejercicio 10
-- existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
-- existeSecuenciaDeAmigos rs us1 us2 | relacionadosDirecto us1 us2 rs == True = True
--                                    | existeSecuenciaDeAmigos rs us1 head(amigosDe us2) = True
--                                    |
-- --                                    | otherwise = existeSecuenciaDeAmigos rs us1 head(tail (amigosDe us2))

-- existeSecuenciaDeAmigos rs us1 us2 | estaEnElMismoCluster rs us1 us2

-- estaEnElMismoCluster rs us1 us2 

-- meter en una lista vacia los amigos de us1, luego los amigos de los que meti si no pertenecen ya. asi hasta que corte y no este us2 o hasta meter us2

-- listaCadenaDeAmigosAux :: RedSocial -> Usuario -> [Usuario] -> [Usuario]
-- listaCadenaDeAmigosAux (us,rs,ps) u1 xs = 

--NO llamo a clusterDeCadenaD;;; con rs u1 [] []

-- clusterDeCadenaDeAmigosDeUnUsuario :: RedSocial -> Usuario -> [Usuario] -> [Usuario] -> [Usuario]
-- clusterDeCadenaDeAmigosDeUnUsuario rs u1 us [] = clusterDeCadenaDeAmigosDeUnUsuario rs u1 [u1] (u1 : (amigosDe u1))
-- clusterDeCadenaDeAmigosDeUnUsuario rs u1 us cluster | us /= [] =
--                                                      clusterDeCadenaDeAmigosDeUnUsuario rs (head(tail nuevoCluster)) (tail cluster) nuevoCluster
--                                                     | otherwise = cluster
--                                                      where nuevoCluster = (agregarSiNoEstan (amigosDe rs u1) cluster)

-- agregarSiNoEstan :: (Eq t) => [t] -> [t] -> [t]
-- agregarSiNoEstan [] ys =  ys
-- agregarSiNoEstan (x:xs) ys | pertenece x ys = agregarSiNoEstan xs ys
--                            | otherwise = (agregarSiNoEstan xs ys) ++ [x]

-- cadenaDeAmigos us1: [amigo us2,us2] 

existeSecuenciaDeAmigosAux :: RedSocial -> Usuario -> Usuario ->[Usuario] -> Bool
existeSecuenciaDeAmigosAux rs u1 u2 cluster | pertenece u2  cluster = True
                                            | mismosElementos cluster (cluster ++ agregarAmigosDeCadaUnoSinRepetidos rs cluster) = False
                                        --     | otherwise = existeSecuenciaDeAmigosAux rs u1 u2 (eliminarRepetidos  (cluster ++ (agregarAmigosDeCadaUnoSinRepetidos rs cluster)))          
                                            | otherwise = existeSecuenciaDeAmigosAux rs u1 u2 ( (cluster ++ (agregarAmigosDeCadaUnoSinRepetidos rs cluster)))          

existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos rs u1 u2  = existeSecuenciaDeAmigosAux rs u1 u2 [u1]



agregarAmigosDeCadaUnoAux :: RedSocial -> [Usuario] -> [Usuario] -> [Usuario]
agregarAmigosDeCadaUnoAux rs [] xs = xs
agregarAmigosDeCadaUnoAux rs (u:us) xs = agregarAmigosDeCadaUnoAux rs (us) ((amigosDe rs u)++xs) 


agregarAmigosDeCadaUnoSinRepetidos :: RedSocial -> [Usuario] -> [Usuario]
agregarAmigosDeCadaUnoSinRepetidos rs us = eliminarRepetidos (agregarAmigosDeCadaUnoAux rs us [])