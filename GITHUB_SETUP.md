# Configuración de GitHub para sbcUtils

## 📝 Pasos para subir el paquete a GitHub

### 1. Crear repositorio en GitHub

1. Ve a https://github.com/new
2. Nombre del repositorio: `sbcUtils`
3. Descripción: `Utilidades para estudios de mercado en SBC`
4. **NO** inicialices con README, .gitignore o licencia (ya los tenemos)
5. Haz clic en "Create repository"

### 2. Conectar repositorio local con GitHub

Ejecuta estos comandos en la terminal:

```bash
cd /Users/jramirez/Documents/EstudiosR/proyectos/sbcUtils

# Configurar remote
git remote add origin https://github.com/mygeorgyboy/sbcUtils.git

# Renombrar rama a main (opcional pero recomendado)
git branch -M main

# Subir archivos
git push -u origin main
```

### 3. Verificar en GitHub

Ve a `https://github.com/mygeorgyboy/sbcUtils` y verifica que todos los archivos estén ahí.

---

## 🔄 Workflow de actualización

Cada vez que hagas cambios al paquete:

### Opción A: Usando el script automatizado

```r
# En R
source("dev/04_deploy.R")
# Te pedirá un mensaje de commit
```

### Opción B: Manualmente en terminal

```bash
cd /Users/jramirez/Documents/EstudiosR/proyectos/sbcUtils

# Ver cambios
git status

# Agregar archivos modificados
git add .

# Crear commit
git commit -m "Descripción de los cambios"

# Subir a GitHub
git push
```

---

## 📦 Instalación del paquete desde GitHub

Una vez subido a GitHub, cualquiera puede instalarlo con:

```r
# Instalar desde GitHub
remotes::install_github("mygeorgyboy/sbcUtils")

# Usar el paquete
library(sbcUtils)
```

---

## 🔐 Credenciales de Git

Si es la primera vez que usas Git en esta máquina:

```bash
# Configurar nombre y email
git config --global user.name "Tu Nombre"
git config --global user.email "tu@email.com"

# Para evitar escribir contraseña cada vez, usar SSH o Personal Access Token
# Ver: https://docs.github.com/es/authentication
```

---

## ✅ Verificación final

Después de subir a GitHub, verifica que:

- [ ] El código está en GitHub
- [ ] El README.md se visualiza correctamente
- [ ] La licencia aparece en el repo
- [ ] Puedes instalar el paquete con `remotes::install_github("mygeorgyboy/sbcUtils")`
