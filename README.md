# TP Arbitraje - Programación Funcional

Bot de arbitraje de criptomonedas desarrollado en Haskell que detecta y ejecuta oportunidades de arbitraje triangular en Binance Testnet.

## Arquitectura

El proyecto sigue una arquitectura en 3 capas:

- **Capa 1: INPUT** - Efectos e Interacción Externa (Binance API)
- **Capa 2: TRANSFORMACIÓN** - Lógica Pura (Detección de arbitraje)
- **Capa 3: OUTPUT** - Orquestación (Stack monádico y estado)

## Setup

### Requisitos
- GHC >= 9.0
- Cabal >= 3.0

### Instalación

1. Instalar dependencias:

```bash
cabal update
cabal build
```

2. Configurar variables de entorno:

```bash
cp .env.example .env
```

3. Editar `.env` con tus API keys de Binance Testnet

### Obtener API Keys de Testnet

1. Visitar https://testnet.binance.vision/
2. Generar API key y secret
3. Copiar en `.env` (Mirar ejemplo de .env.example)

## Uso

```bash
cabal run tp-arbitraje
```

## Bibliotecas Principales

- `transformers` & `mtl` - Stack monádico
- `aeson` - JSON parsing
- `req` - HTTP client
- `cryptonite` - Criptografía HMAC
- `containers` - Estructuras de datos inmutables
- `dotenv` - Configuración
