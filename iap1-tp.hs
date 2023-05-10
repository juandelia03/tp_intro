-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU
module Solucion where


type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

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

-- Funciones Auxiliares
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos x = eliminarRepetidosAux x []

eliminarRepetidosAux :: (Eq t) => [t] -> [t] -> [t]
eliminarRepetidosAux [] ys = reverso ys
eliminarRepetidosAux (x:xs) ys | pertenece x ys = eliminarRepetidosAux xs ys
                               | otherwise = eliminarRepetidosAux xs (x:ys)

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece t [] = False
pertenece t (x:xs) | t == x = True 
                   | otherwise = pertenece t xs

reversoAux :: [t] -> [t] -> [t]
reversoAux [] y = y
reversoAux (x:xs) y = reversoAux xs (x:y)

reverso :: [t] -> [t]
reverso x = reversoAux x []

longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = longitud xs + 1

mismosElementosAux :: (Eq t) => [t] -> [t] -> Bool
mismosElementosAux [] ys = True
mismosElementosAux (x:xs) ys | pertenece x ys = mismosElementosAux xs ys
                             | otherwise = False

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos xs ys = (mismosElementosAux xs ys) && (mismosElementosAux ys xs)

-- Ejercicios

-- nombresDeUsuarios

-- proyectarnNombres toma una lista de usuarios y devuelve una lista 
-- con los nombres de usuario de esos usuarios eliminando repetidos
proyectarNombres :: [Usuario] -> [[Char]]
proyectarNombres [] = []
proyectarNombres (u:us) = eliminarRepetidos((nombreDeUsuario u) : proyectarNombres us)

-- nombresDeUsuarios toma una lista de usuarios de una red social valida
-- y la hace correr en ProyectarNombres para obtener los distintos nombres en una lista
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios rs = proyectarNombres (usuarios rs)

-- amigosDe

-- dada una redsocial y usuario (u) validos devuelve una lista con cada uno de los usuarios los cuales estan relacionados con u (sin repetir)
-- mirando las relaciones en las que esta u y agregando el otro elemento de la tupla a la res (no hay repeticiones ya que en una relacion valida no hay relaciones repetidas)
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (us,[],ps) u = []
amigosDe (us,(r:rs),ps) u | fst r == u = snd r : amigosDe (us,rs,ps) u
                          | snd r == u = fst r : amigosDe (us,rs,ps) u
                          | otherwise = amigosDe (us,rs,ps) u

-- cantidadDeAmigos

-- dada una redS y usuario (u) validos devuelve la cantidad de amigos de u 
-- mirando cuantos elementos sin repetir que tiene la lista de amigos de u (funcion definida arriba)
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos rs u = fromInteger(longitud((amigosDe rs u)))
-- usamos fromInteger porque longitud esta definido para Integer y no Int

-- usuarioConMasAmigos

-- dada una redS y usuario (u) validos  mira si el primer usuario de la lista de usuarios tiene mas amigos que todo el resto. si no es asi lo saca 
-- y chequea con el siguiente hasta encontrarlo, cuando queda solo un usuario en la lista es el que mas amigos tiene, pues ya corroboro que ningun otro es el que mas tiene
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos ([u1],rs,ps) = u1
usuarioConMasAmigos ((u:us),rs,ps) | (cantidadDeAmigos ((u:us),rs,ps) u) >= (cantidadDeAmigos ((u:us),rs,ps) (usuarioConMasAmigos (us,rs,ps))) = u
                                   | otherwise = usuarioConMasAmigos (us,rs,ps)


-- estaRobertoCarlos

-- dada una red social valida chequea si el primer usuario tiene mas de 1M de amigos (cantidadDeAmigos), si no chequea con el siguiente
-- hasta que alguno cumpla (Caso True) o hasta llegar a la lista vacia (Caso False)
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([],rs,ps) = False
estaRobertoCarlos ((u:us),rs,ps) | cantidadDeAmigos ((u:us),rs,ps) u > 1000000 = True
                                 | otherwise = estaRobertoCarlos (us,rs,ps)


-- publicacionesDe

-- dada una redS y usuario (u) validos chequea para cada publicacion si el usuarioDePublicacion es el mismo que el usuario de quien queremos recuperar todas sus publicaciones,
-- en caso True agrega la publicacion a res en Caso False sigue chequeando asi hasta llegar a la lista vacia de publicaciones y luego devuelve la lista de todas las publicaciones de el u (no hay repetidas ya que en una RedSocialValida no hay repetidas)
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (us,rs,[]) u = []
publicacionesDe (us,rs,(p:ps)) u | usuarioDePublicacion p == u = p : (publicacionesDe (us,rs,ps) u)
                                 | otherwise = publicacionesDe (us,rs,ps) u

-- publicacionesQueLeGustanA

-- dada una redS y usuario (u) valido devuelve una lista con todas las publicaciones que le gustan a u. Chequea que u pertenezca a los likes de cada publicacion de la redS,
-- si pertenece agrega esa publicacion a la res, si no no la agrega y sigue chequeando. asi hasta llegar a la lista vacia de publicaciones. (no hay repetidas porque requiere RedSocialValida)
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (us,rs,[]) u = []
publicacionesQueLeGustanA (us,rs,(p:ps)) u | pertenece u (likesDePublicacion p) = p : (publicacionesQueLeGustanA (us,rs,ps) u) 
                                           | otherwise = (publicacionesQueLeGustanA (us,rs,ps) u) 

--lesGustanLasMismasPublicaciones

-- dada una RedS u1 u2 validos chequea que a u1 y u2 le gusten exactamente las mismas publicaciones
-- para eso basta ver si las publicaciones de la red que le gustan a cada uno son las mismas
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones rs u1 u2 = mismosElementos (publicacionesQueLeGustanA rs u1) (publicacionesQueLeGustanA rs u2)

-- tieneUnSeguidorFiel

contenidoPrimeraEnSegunda :: (Eq t) => [t] -> [t] -> Bool
contenidoPrimeraEnSegunda [] ys = True
contenidoPrimeraEnSegunda (x:xs) ys | pertenece x ys = contenidoPrimeraEnSegunda xs ys
                                    | otherwise = False

-- dado una RedS y u validos chequea si existe algun u2 (que no puede ser u) que le gusten todas las publicaciones de u (puede que le gusten otras p tambien)
-- para eso basta ver que todas las publicaciones de u estan contenidas en las que le gustan  a u2 (u2 no esta dado asi que se chequea uno a uno en la lista de todos los usuarios si alguno cumple esa condicion)
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel ([],rs,ps) u1 = False
tieneUnSeguidorFiel (u:us,rs,ps) u1 | u == u1 = tieneUnSeguidorFiel (us,rs,ps) u1
                                    | contenidoPrimeraEnSegunda (publicacionesDe (u:us,rs,ps) u1) (publicacionesQueLeGustanA (u:us,rs,ps) u) = True
                                    | otherwise = tieneUnSeguidorFiel (us,rs,ps) u1
                                    --where red = (u:us,rs,ps)

--existeSecuenciaDeAmigos

-- dada una RedS u1 u2 validos corrobora si existe alguna secuencia de usuarios que empiece con u1 termine con u2
-- y que sea una cadena de amigos (cada usuario esta directamente relacionado con el siguiente)
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos rs u1 u2  = existeSecuenciaDeAmigosAux rs u1 u2 [u1]

-- la funcion chequea que al agregar a todos los amigos de cada uno de los usuarios de una lista a esa misma lista (y asi sucesivamente) seria posible formar una cadena
-- en el caso de que (empezando la lista con el u1) en algun momento aparezca el u2 en la lista, ya que solamente agregando a los amigos de u1 y los amigos de sus amigos (y asi sucesivamente)
-- apareceria u2 , en el caso  de que se busquen los amigos de cada uno en la lista y al agregarse esos usuarios a la lista ambas tengan los mismo elementos, significaria que no hay mas cadenas posibles para formar
-- y si no aparecio u2 entonces no existe ninguna secuenciaDeAmigos que empiece con u1 y llegue a u2. En el caso de que u2 pertenezca a la secuencia significa que existe alguna cadena de amigos de u1 a u2
existeSecuenciaDeAmigosAux :: RedSocial -> Usuario -> Usuario ->[Usuario] -> Bool
existeSecuenciaDeAmigosAux rs u1 u2 cluster | pertenece u2  cluster = True
                                            | mismosElementos cluster (cluster ++ agregarAmigosDeCadaUnoSinRepetidos rs cluster) = False
                                        --     | otherwise = existeSecuenciaDeAmigosAux rs u1 u2 (eliminarRepetidos  (cluster ++ (agregarAmigosDeCadaUnoSinRepetidos rs cluster)))          
                                            | otherwise = existeSecuenciaDeAmigosAux rs u1 u2 ( (cluster ++ (agregarAmigosDeCadaUnoSinRepetidos rs cluster)))          

agregarAmigosDeCadaUnoAux :: RedSocial -> [Usuario] -> [Usuario] -> [Usuario]
agregarAmigosDeCadaUnoAux rs [] xs = xs
agregarAmigosDeCadaUnoAux rs (u:us) xs = agregarAmigosDeCadaUnoAux rs (us) ((amigosDe rs u)++xs) 


agregarAmigosDeCadaUnoSinRepetidos :: RedSocial -> [Usuario] -> [Usuario]
agregarAmigosDeCadaUnoSinRepetidos rs us = eliminarRepetidos (agregarAmigosDeCadaUnoAux rs us [])