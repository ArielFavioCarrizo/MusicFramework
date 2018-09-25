Compilar EsferixisMusicFrameworkNative con Visual Studio como EMFB_STK.DLL, modificar la ruta apuntada por el archivo '.ghci'
a la que corresponde con el binding compilado (EMFB_STK.DLL).

Iniciar ghci en el directorio.
Si no funciona ver '.ghci'

Luego:
:load Esferixis.MusicFramework.Backend.STK

Dependencias:

mutable-containers
