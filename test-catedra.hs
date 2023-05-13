module Test where

import Solucion
import Test.HUnit

main = runTestTT tests

-- tests = test [
--     " nombresDeUsuarios 1" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"],

--     " amigosDe 1" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4],

--     " cantidadDeAmigos 1" ~: (cantidadDeAmigos redA usuario1) ~?= 2,

--     " usuarioConMasAmigos 1" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4],

--     " estaRobertoCarlos 1" ~: (estaRobertoCarlos redA) ~?= False,

--     " publicacionesDe 1" ~: (publicacionesDe redA usuario2) ~?= [publicacion2_1, publicacion2_2],

--     " publicacionesQueLeGustanA 1" ~: (publicacionesQueLeGustanA redA usuario1) ~?= [publicacion2_2, publicacion4_1],

--     " lesGustanLasMismasPublicaciones 2" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True,

--     " tieneUnSeguidorFiel 1" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True,

--     " existeSecuenciaDeAmigos 1" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True
--  ]

tests = test [
    "nombresDeUsuarios" ~: testsNombresDeUsuarios,
    "amigosDe" ~: testsAmigosDe, 
    "cantidadDeAmigos" ~: testsCantidadDeAmigos,
    "usuarioConMasAmigos" ~: testsUsuarioConMasAmigos,
    "estaRobertoCarlos" ~: testsEstaRobertoCarlos,
    "publicacionesDe" ~: testsPublicacionesDe,
    "publicacionesQueLeGustanA" ~: testsPublicacionesQueLeGustanA,
    "lesGustanLasMismasPublicaciones" ~: testsLesGustanLasMismasPublicaciones,
    "tieneUnSeguidorFiel" ~: testsTieneUnSeguidorFiel,
    "existeSecuenciaDeAmigos" ~: testsExisteSecuenciaDeAmigos
 ]



testsNombresDeUsuarios = test [
    nombresDeUsuarios ([],[],[]) ~?= [],
    nombresDeUsuarios([(1,"martina"),(2,"tato")],[],[]) ~?= ["martina","tato"],
    nombresDeUsuarios([(1,"martina"),(2,"martina")],[],[]) ~?= ["martina"]
    -- nombresDeUsuarios redA ~?= ["Juan","Natalia","Pedro","Mariela"]
 ]

testsAmigosDe = test [
    -- amigosDe ([usuario1],[],[]) usuario1 ~?= [],
    -- amigosDe redA usuario1 ~?= [usuario2, usuario4]
    amigosDe ([(3,"juan")],[],[]) (3,"juan") ~?= [],
    amigosDe ([(3,"juan"),(1,"martina"),(4,"juanma")],[((4,"juanma"),(1,"martina"))],[]) (3,"juan") ~?= [],
    amigosDe ([(3,"juan"),(1,"martina"),(4,"juanma")],[((3,"juan"),(1,"martina"))],[]) (3,"juan") ~?= [(1,"martina")],
    amigosDe ([(3,"juan"),(1,"martina"),(4,"juanma")],[((1,"martina"),(3,"juan"))],[]) (3,"juan") ~?= [(1,"martina")]
 ]

testsCantidadDeAmigos = test [
    cantidadDeAmigos ([(4,"juanma")],[],[]) (4,"juanma") ~?= 0,
    cantidadDeAmigos ([(4,"juanma"),(1,"martina"),(3,"juan")],[((1,"martina"),(3,"juan"))],[]) (4,"juanma") ~?= 0,
    cantidadDeAmigos ([(4,"juanma"),(1,"martina"),(3,"juan"),(2,"tato")],[((1,"martina"),(3,"juan")),((4,"juanma"),(2,"tato")),((1,"martina"),(4,"juanma"))],[]) (4,"juanma") ~?= 2
 ]

testsUsuarioConMasAmigos = test [
    -- expectAny (usuarioConMasAmigos redA) [usuario2, usuario4] -- espera que sea u2 o u4
    expectAny (usuarioConMasAmigos ([(4,"juanma"),(2,"tato")],[],[]))  [(4,"juanma"),(2,"tato")],     
    expectAny (usuarioConMasAmigos ([(4,"juanma"),(2,"tato"),(3,"juan"),(1,"martina"),(5,"menem")],[((4,"juanma"),(2,"tato")),((4,"juanma"),(1,"martina")),((3,"juan"),(2,"tato"))],[])) [(4,"juanma"),(2,"tato")],
    usuarioConMasAmigos ([(4,"juanma"),(2,"tato"),(3,"juan"),(1,"martina"),(5,"menem")],[((4,"juanma"),(2,"tato")),((4,"juanma"),(1,"martina")),((3,"juan"),(5,"menem"))],[]) ~?= (4,"juanma") 
 ]

testsEstaRobertoCarlos = test [
    estaRobertoCarlos ([],[],[]) ~?= False,
    estaRobertoCarlos ([(3,"juan"),(1,"martina")],[],[]) ~?= False,
    estaRobertoCarlos ([(3,"juan"),(1,"martina")],[],[]) ~?= False,
    estaRobertoCarlos ([(3,"juan"),(1,"martina")],[((3,"juan"),(1,"martina"))],[]) ~?= False,
    estaRobertoCarlos ([(3,"juan"),(1,"martina"),(2,"tato"),(4,"juanma"),(5,"menem"),(6,"macri"),(7,"nestor"),(8,"peron"),(9,"roca"),(10,"frondizi"),(11,"illia"),(12,"cerati")],[((3,"juan"),(1,"martina")),((8,"peron"),(5,"menem")),((8,"peron"),(6,"macri")),((8,"peron"),(7,"nestor")),((8,"peron"),(9,"roca")),((8,"peron"),(10,"frondizi")),((8,"peron"),(11,"illia")),((8,"peron"),(12,"cerati")),((8,"peron"),(4,"juanma")),((8,"peron"),(2,"tato")),((8,"peron"),(3,"juan")),((8,"peron"),(1,"martina"))],[]) ~?= True

 ]

testsPublicacionesDe = test [
    -- publicacionesDe redA usuario2 ~?= [publicacion2_1, publicacion2_2]
    publicacionesDe ([(1,"martina")],[],[]) (1,"martina") ~?= [],
    publicacionesDe ([(1,"martina"),(2,"tato")],[],[((2,"tato"), "Este es mi primer post", [])]) (1,"martina") ~?= [],
    publicacionesDe ([(1,"martina"),(2,"tato")],[],[((2,"tato"), "Este es mi primer post", []),((1,"martina"), "hola loco", [])]) (1,"martina") ~?= [((1,"martina"), "hola loco", [])]
 ]

testsPublicacionesQueLeGustanA = test [
    -- publicacionesQueLeGustanA redA usuario1 ~?= [publicacion2_2, publicacion4_1]
    publicacionesQueLeGustanA ([(1,"martina")],[],[]) (1,"martina") ~?= [],
    publicacionesQueLeGustanA ([(1,"martina")],[],[((2,"tato"), "Este es mi primer post", [])]) (1,"martina") ~?= [],
    publicacionesQueLeGustanA ([(1,"martina")],[],[((2,"tato"), "Este es mi primer post", [(1,"martina")])]) (1,"martina") ~?= [((2,"tato"), "Este es mi primer post", [(1,"martina")])],
    publicacionesQueLeGustanA ([(1,"martina")],[],[((2,"tato"), "Este es mi primer post", [(1,"martina")]),((1,"martina"), "hola loco", [])]) (1,"martina") ~?= [((2,"tato"), "Este es mi primer post", [(1,"martina")])]
 ]

testsLesGustanLasMismasPublicaciones = test [
    lesGustanLasMismasPublicaciones ([usuario1,usuario2],[],[publicacion1_2]) usuario1 usuario2 ~?= True,
    lesGustanLasMismasPublicaciones ([usuario1,usuario2],[],[]) usuario1 usuario2 ~?= True,
    lesGustanLasMismasPublicaciones ([usuario1,usuario2,usuario4],[],[publicacion2_2]) usuario1 usuario2 ~?= False,
    lesGustanLasMismasPublicaciones ([usuario1,usuario2,usuario4],[],[publicacion1_1]) usuario1 usuario2 ~?= False,
    lesGustanLasMismasPublicaciones ([usuario1,usuario2,usuario4],[],[publicacion2_2,publicacion1_1,publicacion4_1]) usuario1 usuario2 ~?= False,
    lesGustanLasMismasPublicaciones ([usuario1,usuario2,usuario4],[],[publicacion2_2,publicacion1_1]) usuario1 usuario2 ~?= False,
    lesGustanLasMismasPublicaciones ([usuario1,usuario2,usuario5],[],[publicacion4_1,publicacion1_5]) usuario1 usuario2 ~?= True
 ]


testsTieneUnSeguidorFiel = test [
    tieneUnSeguidorFiel ([usuario1],[],[]) usuario1 ~?= False,
    tieneUnSeguidorFiel ([usuario1, usuario2, usuario4],[],[publicacion1_4, publicacion2_1]) usuario1 ~?= False,
    tieneUnSeguidorFiel ([usuario1,usuario2,usuario4],[],[publicacion1_4,publicacion1_1, publicacion2_1]) usuario1 ~?= False,
    tieneUnSeguidorFiel ([usuario1,usuario2,usuario4,usuario5],[],[publicacion1_5,publicacion1_1, publicacion2_1]) usuario1 ~?= False,
    tieneUnSeguidorFiel ([usuario1, usuario2,usuario4],[],[publicacion1_1,publicacion1_2,publicacion2_1]) usuario1 ~?= True
 ]

testsExisteSecuenciaDeAmigos = test [
    existeSecuenciaDeAmigos ([(1,"martina"),(2,"tato")],[],[]) (1,"martina") (2,"tato") ~?= False,
    existeSecuenciaDeAmigos ([(1,"martina"),(2,"tato"),(8,"peron"),(5,"menem")],[((8,"peron"),(5,"menem"))],[]) (1,"martina") (2,"tato") ~?= False,
    existeSecuenciaDeAmigos ([(1,"martina"),(2,"tato"),(3,"juan"),(8,"peron"),(5,"menem")],[((1,"martina"),(3,"juan")),((8,"peron"),(5,"menem"))],[]) (1,"martina") (2,"tato") ~?= False,
    existeSecuenciaDeAmigos ([(1,"martina"),(2,"tato"),(3,"juan"),(8,"peron"),(5,"menem")],[((2,"tato"),(3,"juan")),((8,"peron"),(5,"menem"))],[]) (1,"martina") (2,"tato") ~?= False,
    existeSecuenciaDeAmigos ([(1,"martina"),(2,"tato"),(3,"juan"),(4,"juanma"),(8,"peron"),(5,"menem")],[((2,"tato"),(3,"juan")),((1,"martina"),(4,"juanma")),((8,"peron"),(5,"menem"))],[]) (1,"martina") (2,"tato") ~?= False,
    existeSecuenciaDeAmigos ([(1,"martina"),(2,"tato"),(3,"juan"),(4,"juanma"),(8,"peron"),(5,"menem")],[((2,"tato"),(3,"juan")),((1,"martina"),(4,"juanma")),((4,"juanma"),(2,"tato")),((8,"peron"),(5,"menem"))],[]) (1,"martina") (3,"juan") ~?= True,
    existeSecuenciaDeAmigos ([(1,"martina"),(2,"tato"),(3,"juan"),(4,"juanma"),(8,"peron"),(5,"menem")],[((2,"tato"),(3,"juan")),((1,"martina"),(4,"juanma")),((4,"juanma"),(2,"tato")),((8,"peron"),(5,"menem"))],[]) (3,"juan") (1,"martina")  ~?= True

 ]


expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

--ejemplos testing 1

