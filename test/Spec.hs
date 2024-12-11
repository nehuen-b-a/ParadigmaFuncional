import Test.Hspec
import TP

demonioDePrueba = Demonio { 
  deudores = ["Pedro", "Juan"], 
  subordinados = [] 
}

demonioDePrueba1 = Demonio { 
  deudores = ["Luis", "Juan"], 
  subordinados = [ 
    Demonio { 
      deudores = ["Gabriel", "Pedro"], 
      subordinados = [] 
    },
    Demonio { 
      deudores = ["Agustin", "Camila"], 
      subordinados = [] 
    }
  ]
}

demonioDePrueba2 = Demonio { 
  deudores = ["Juan"], 
  subordinados = [ 
    Demonio { 
      deudores = ["Gabriel"], 
      subordinados = [] 
    },
    Demonio { 
      deudores = ["Luis", "Camila"], 
      subordinados = [
        Demonio { 
          deudores = ["Pepe"], 
          subordinados = [
            Demonio { 
              deudores = ["Pedro"], 
              subordinados = []
            }
          ]
        }
      ] 
    }
  ]
}

demonioDePrueba3 = Demonio { 
  deudores = ["Juan"], 
  subordinados = [ 
    Demonio { 
      deudores = ["Gabriel"], 
      subordinados = [] 
    },
    Demonio { 
      deudores = ["Luis", "Camila"], 
      subordinados = [
        Demonio { 
          deudores = ["Pepe"], 
          subordinados = [
            Demonio { 
              deudores = ["Cristian"], 
              subordinados = [
                Demonio { 
                  deudores = ["Nacho"], 
                  subordinados = []
                }
              ] 
            }
          ] 
        }
      ]
    }
  ]
}

main :: IO ()
main = hspec $ do
  describe "Punto 1" $ do
    it "Cuando un humano desea La paz mundial se vuelve 20 veces más feliz:" $ do
       (felicidad . laPazMundial) humanoDePrueba `shouldBe` 2000
    it "Cuando un humano desea Recibirse de una carrera la felicidad aumenta en 250:" $ do
       (felicidad . recibirseDe "Medicina") humanoDePrueba `shouldBe` 350
    it "Cuando un humano desea Recibirse de |Medicina| el reconocimiento aumenta en 3 veces la longitud de la carrera: " $ do
       (reconocimiento . recibirseDe "Medicina") humanoDePrueba `shouldBe` 74
    it "Cuando un humano desea Ser famoso la felicidad se fija en 50:" $ do
       (felicidad . serFamoso) humanoDePrueba `shouldBe` 50
    it "Cuando un humano desea Ser famoso el reconocimiento aumenta en 1000:" $ do
       (reconocimiento . serFamoso) humanoDePrueba `shouldBe` 1050

  describe "Punto 2" $ do
    it "La espiritualidad para el humano de prueba, del deseo de recibirse de la carrera de medicina, seria 226" $ do
       espiritualidad humanoDePrueba (recibirseDe "Medicina")  `shouldBe` 226

  describe "Punto 3" $ do
    it "La felicidad del humano de prueba al cumplir sus todos sus deseos no mejora" $ do
      mejoraFelicidadAlCumplirDeseos humanoDePrueba `shouldBe` False  
  
  describe "Punto 4" $ do
    it "Los deseos mas terrenales del humano de prueba serian ser famoso y recibirse de ingeniero" $ do
      (felicidad.(foldl aplicarDeseo humanoDePrueba).deseosMasTerrenales) humanoDePrueba `shouldBe` (felicidad.recibirseDe "Ingeniería en Sistemas de Información".serFamoso) humanoDePrueba
    it "La mejor version del humano de prueba resulta de cumplir su deseo de paz mundial" $ do
      (felicidad.mejorVersion) humanoDePrueba `shouldBe` (felicidad.laPazMundial) humanoDePrueba
  
  describe "Punto 5" $ do
    it "Cuando un humano es deudor de un Demonio" $ do
      tienePoderSobre demonioDePrueba humanoDePrueba `shouldBe` True
    it "Cuando un humano es deudor de un subordinado Directo" $ do
      tienePoderSobre demonioDePrueba1 humanoDePrueba `shouldBe` True
    it "Cuando un humano es deudor de un subordinado indirecto" $ do
      indirectosTienePoderSobre  demonioDePrueba2 humanoDePrueba `shouldBe` True
    it "Cuando un humano no es deudor de un subordinado indirecto" $ do
      indirectosTienePoderSobre demonioDePrueba3 humanoDePrueba `shouldBe` False
  
  describe "Punto 6" $ do
    it "Un demonio que no tiene poder sobre el humano de prueba le cumple el deseo de ser famoso" $ do
      felicidad (fst(ayudarSiLeConviene humanoDePrueba demonioDePrueba3)) `shouldBe` felicidad (serFamoso humanoDePrueba)
    it "El nombre del humano de prueba se incorpora a los deudores del demonio" $ do
      deudores (snd(ayudarSiLeConviene humanoDePrueba demonioDePrueba3)) `shouldBe` deudores (asignarDeudor demonioDePrueba3 humanoDePrueba)