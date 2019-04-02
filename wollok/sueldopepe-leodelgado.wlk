object pepe {
	//const empleado = pepe //No se puede solo usando Atributos xq si hago empleado.faltas() chau. Y para ver eso en los bonos, 
	//el objeto bono deberia tener la const. empleado. Y ahi ya no me sirve para otras personas. Entonces la unica 
	//opcion que me queda es parametrizar. Parametrizo los metodos de los bonos y se lo paso con SELF.
	var faltas = 0
	var categoria = gerente
	var bonoResultados = bonoNulo //Lo hago por objetos y no por variables xq si quiero agregar un bono nuevo, tengo que
	//volver a tocar a Pepe y es una paja. Mas rapido y facil crear un objeto nuevo y alv.
	var bonoPresentismo = bonoNulo
	//sueldo = neto + bono x presentismo + bono x resultados
	
	method faltas() { return faltas }
	
	//Setter
	method categoria(_categoria) {
		categoria = _categoria
	}
	
	//Setter
	method bonoResultados(_bonoResultados) {
		bonoResultados = _bonoResultados
	}
	
	method bonoPresentismo(_bonoPresentismo) {
		bonoPresentismo = _bonoPresentismo
	}
	
	method sueldo() {
		return self.sueldoNeto() +
		self.sueldoPorResultados() +
		self.sueldoPorPresentismo()
	}
	
	method sueldoNeto() {
		return categoria.sueldoNeto() //Pepe delega en su categoria el saber el sueldo neto
		//La categoria es la que sabe el sueldo neto, no pepe. El neto DEPENDE de la categoria(asi dice el enunciado), entonces
		//es logico que el valor del sueldo neto este en la categoria, y no en pepe.
	}
	
	method sueldoPorResultados() {
	    return bonoResultados.sueldoPorResultados(self)
	}
	
	method sueldoPorPresentismo() {
    //Normal: $2000 pesos si la persona a quien se aplica no faltó nunca, $1000 si faltó sólo un día, $0 en cualquier otro caso.
    //Ajuste: $100 pesos si el empleado no faltón nunca, $0 en cualquier otro caso.
    //Demagógico: $500 pesos si el neto es menor a 18000, $300 en caso contrario. Para este bono no importa cuántas veces faltó el emplado.
    //Nulo: nada.
    	return bonoPresentismo.sueldoPorPresentismo(self)
	}
}

object gerente {
	method sueldoNeto() { return 15000 }
}

object cadete {
	method sueldoNeto() { return 20000 }
}


object bonoPorcentual {
	method sueldoPorResultados(empleado) { //Este es un acoplamiento de Pepe, o de cualquier empleado
		//return pepe.sueldoNeto() * 0.1 //Opcion Hardcodeada (solo funciona para pepe)
		return empleado.sueldoNeto() * 0.1 
	}
}

object bonoFijo {
	method sueldoPorResultados(empleado) { //Este es un acoplamiento de Pepe, o de cualquier empleado
		return 800 }
}

object bonoNulo {
	method sueldoPorResultados(empleado){ //Este es un acoplamiento de Pepe, o de cualquier empleado
		return 0
	}
	method sueldoPorPresentismo(empleado) { //Este es un acoplamiento de Pepe, o de cualquier empleado
		return 0
	}
}

object bonoPresentismoNormal {
	method sueldoPorPresentismo(empleado) { //Este es un acoplamiento de Pepe, o de cualquier empleado
	if (empleado.faltas() == 0) { return 2000 }
	else if (empleado.faltas() == 1) { return 1000 }
	else  { return 0 } 
	}
}

object bonoPresentismoAjuste {
	method sueldoPorPresentismo(empleado) { //Este es un acoplamiento de Pepe, o de cualquier empleado
	if (empleado.faltas() == 0) { return 100 }
	else { return 0 }
	}
}

object bonoPresentismoDemagogico {
	method sueldoPorPresentismo(empleado) { //Este es un acoplamiento de Pepe, o de cualquier empleado
	if (empleado.sueldoNeto() < 18000) { return 500 }
	else return 300
	}
}

object sofia {

	var categoria = gerente
	var bonoResultados = bonoNulo
	
	//Setter
	method categoria(_categoria) {
		categoria = _categoria
	}
	
	//Setter
	method bonoResultados(_bonoResultados) {
		bonoResultados = _bonoResultados
	}
	
	method sueldo() {
		return (self.sueldoNeto() * 1.3) +
		self.sueldoPorResultados()
	}
	
	method sueldoNeto() {
		return categoria.sueldoNeto() //Pepe delega en su categoria el saber el sueldo neto
		//La categoria es la que sabe el sueldo neto, no pepe. El neto DEPENDE de la categoria(asi dice el enunciado), entonces
		//es logico que el valor del sueldo neto este en la categoria, y no en pepe.
	}
	
	method sueldoPorResultados() {
	    return bonoResultados.sueldoPorResultados(self)
	}
	
}

object vendedor {
	
	var aumentoPorMuchasVentas = true 
	
	method sueldoNeto() { return 16000 + self.aplicarAumento() }
	
	method aplicarAumento() {
		if (aumentoPorMuchasVentas)
			{return 16000 * 0.25}
		else return 0
	}
	
	method activarAumentoPorMuchasVentas() { aumentoPorMuchasVentas = true }
	
	method desactivarAumentoPorMuchasVentas() { aumentoPorMuchasVentas = false }
	
	
}