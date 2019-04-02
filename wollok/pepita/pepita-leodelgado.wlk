object pepita {
	const costoFijoDeVuelo = 10
	const costoVariableDeVuelo = 1

	var energia = 0
	
	//GETTER: Metodo que retorna lo que yo estoy referenciando en un atributo 
	method energia() { return energia }
	method come(gramos) { energia += 4 * gramos }
	method comer(comida) { energia += comida.energia() }
	method comer(comida,gramos) { energia += comida.energia() * gramos }
	method volar(kms) { energia -= costoFijoDeVuelo + (costoVariableDeVuelo * kms) }
 
	method estaDebil() { return energia < 50 }
	method estaFeliz() { return energia.between(500,1000) }
	method cuantoQuiereVolar() { return( (energia/5) + self.energiaEntre300y400() + self.energiaMultiploDe20() ) }
	
	method energiaEntre300y400() { 
		if (energia.between(300,400)) { return 10 }
		else return 0 
	}

	method energiaMultiploDe20() {
		//var ret = 0 //MAL!!
		if ((energia % 20) == 0) { return 15 }
		else return 0
	}

	method salirAComer() { 
		self.volar(5)
		self.comer(alpiste)
		self.volar(5)
	}
	
	method haceLoQueQuieras() {
		//si en el enunciado dice "si no", y si ambos serian exclusivos, hay que poner "Else If <etc>"
		if (self.estaDebil()) { self.comer(alpiste) }
		if (self.estaFeliz()) { self.volar(8) }
	}
}

object alpiste { 
	const energia = 4 //joules
	method energia() { return energia }
}


object mondongo {
	const energia = 100  //joules
	method energia() { return energia }
}

object alcaucil {
	const energia = 20 //joules
	method energia() { return energia }
}

object sorgo {
	const energia = 9
	method energia() { return energia }
}

object mijo {
	var energia = 15
	
	method energia() { return energia }
	method mojarse() { energia = 15 }
	method secarse() { energia = 20 }
}

object canelones {
	var energia = 20
	method ponerSalsa() { energia += 5 } //25, +queso 32
	method sacarSalsa() { energia -= 5 }
	method ponerQueso() { energia += 7 }//27, +salsa 32
	method sacarQueso() { energia -= 7 }	
}

object roque {
	var pupilo = pepita //<??> Cual es el caso por defecto? Que deberia poner? Deberia ser un param. de roque??
	
	//SETTER: Metodo que intercambia UN, Y SOLO UN atributo, por el que le paso por parametro
	method tuPupiloEs(ave) { pupilo = ave }
	
	method entrenar() {
		pupilo.volar(10)
		pupilo.comer(alpiste, 300) 
		pupilo.volar(5)
		pupilo.haceLoQueQuieras()
	}
}

object pepon {
	var energia = 0
	const costoFijoDeVuelo = 1
	const costoVariableDeVuelo = 0.5
	
	//method comer(comida) { energia += comida.energia() / 2 } Creo que no hace falta.
	method comer(comida, gramos) { energia += comida.energia() / 2 }
	method volar(kms) { energia -= costoFijoDeVuelo + (costoVariableDeVuelo * kms) }
	method haceLoQueQuieras() {
		self.volar(1)
	}
}

object pipa {
	var kmsRecorridos = 0
	var gramosConsumidos = 0
	 
	method kmRecorridos() { return kmsRecorridos }
	method gramosConsumidos() { return gramosConsumidos }

	method comer(comida, gramos) { gramosConsumidos += gramos }
	method volar(km) { kmsRecorridos += km }
	method haceLoQueQuieras() {}
}

