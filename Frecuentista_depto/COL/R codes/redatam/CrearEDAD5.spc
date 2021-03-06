DEFINE PERSONA.EDAD5 
	AS RECODE   PERSONA.P_EDAD
	(0 : 14 = 1)
	(15 : 29 = 2)
	(30 : 44 = 3)
	(45 : 64 = 4)
	(65 : 200 = 5)
	TYPE INTEGER
 RANGE 1:5
SAVE "Frecuentista_deptoV:\DAT\SAECEPAL\Temp\SAE-IPM\Frecuentista_depto\COL\R codes\redatam\EDAD5.rbf" OVERWRITE

FREQ PERSONA.EDAD5

 // IPM Material

DEFINE VIVIENDA.ipm_Material
 AS SWITCH 
 	INCASE VIVIENDA.V_MAT_PARED <= 3 OR VIVIENDA.V_MAT_PISO <=3	ASSIGN 0
  ELSE 1
TYPE INTEGER
RANGE 0:1
SAVE "Frecuentista_deptoV:\DAT\SAECEPAL\Temp\SAE-IPM\Frecuentista_depto\COL\R codes\redatam\ipm_Material.rbf" OVERWRITE

  FREQ VIVIENDA.ipm_Material
// IPM Hacinamiento

DEFINE HOGAR.ipm_Hacinamiento
 AS SWITCH 
 	INCASE HOGAR.HA_TOT_PER / HOGAR.H_NRO_DORMIT > 2 ASSIGN 1
  ELSE 0
TYPE INTEGER
RANGE 0:1
SAVE "Frecuentista_deptoV:\DAT\SAECEPAL\Temp\SAE-IPM\Frecuentista_depto\COL\R codes\redatam\ipm_Hacinamiento.rbf" OVERWRITE
 FREQ HOGAR.ipm_Hacinamiento
 
// IPM Agua 
DEFINE HOGAR.ipm_Agua
 AS SWITCH 
 	INCASE  CLASE.AREA = 1 and HOGAR.H_AGUA_COCIN > 1 ASSIGN 1
    INCASE  CLASE.AREA = 2 and HOGAR.H_AGUA_COCIN > 2 ASSIGN 1
  ELSE 0
TYPE INTEGER
RANGE 0:1
SAVE "Frecuentista_deptoV:\DAT\SAECEPAL\Temp\SAE-IPM\Frecuentista_depto\COL\R codes\redatam\ipm_Agua.rbf" OVERWRITE
  FREQ HOGAR.ipm_Agua
// IPM Saneamiento 
DEFINE VIVIENDA.ipm_Saneamiento
 AS SWITCH 
 	INCASE  CLASE.AREA = 1 and VIVIENDA.V_TIPO_SERSA > 2 ASSIGN 1
    INCASE  CLASE.AREA = 2 and VIVIENDA.V_TIPO_SERSA > 2 ASSIGN 1
  ELSE 0
TYPE INTEGER
RANGE 0:1
SAVE "Frecuentista_deptoV:\DAT\SAECEPAL\Temp\SAE-IPM\Frecuentista_depto\COL\R codes\redatam\ipm_Saneamiento.rbf" OVERWRITE
     FREQ VIVIENDA.ipm_Saneamiento
// IPM EnergÍa 
DEFINE VIVIENDA.ipm_Energia
 AS SWITCH 
 	  INCASE   VIVIENDA.VD_GAS = 1 AND  VIVIENDA.VA_EE = 1 ASSIGN 0
  ELSE 1
TYPE INTEGER
RANGE 0:1
SAVE "Frecuentista_deptoV:\DAT\SAECEPAL\Temp\SAE-IPM\Frecuentista_depto\COL\R codes\redatam\ipm_Energia.rbf" OVERWRITE
     FREQ VIVIENDA.ipm_Energia
// IPM Internet 
DEFINE VIVIENDA.ipm_Internet
 AS SWITCH 
 	  INCASE    VIVIENDA.VF_INTERNET = 1 ASSIGN 0
  ELSE 1
TYPE INTEGER
RANGE 0:1
SAVE "Frecuentista_deptoV:\DAT\SAECEPAL\Temp\SAE-IPM\Frecuentista_depto\COL\R codes\redatam\ipm_Internet.rbf" OVERWRITE
      FREQ VIVIENDA.ipm_Internet
// IPM Educacion 
DEFINE PERSONA.ipm_Educacion
 AS SWITCH 
 	  INCASE PERSONA.PA_ASISTENCIA = 2 and PERSONA.ANEST < 11 and (PERSONA.P_EDAD >= 18 and PERSONA.P_EDAD <= 19) ASSIGN 1
      INCASE (PERSONA.P_EDAD >= 20 and PERSONA.P_EDAD <= 29) and  PERSONA.ANEST < 11  ASSIGN 1
      INCASE (PERSONA.P_EDAD >= 30 and PERSONA.P_EDAD <= 59) and  PERSONA.ANEST < 9  ASSIGN 1
      INCASE PERSONA.P_EDAD >= 60  and  PERSONA.ANEST < 4  ASSIGN 1
  ELSE 0
TYPE INTEGER
RANGE 0:1
SAVE "Frecuentista_deptoV:\DAT\SAECEPAL\Temp\SAE-IPM\Frecuentista_depto\COL\R codes\redatam\ipm_Educacion.rbf" OVERWRITE
      FREQ PERSONA.ipm_Educacion



   




 