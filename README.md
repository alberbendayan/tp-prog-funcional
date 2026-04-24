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

### Obtener Telegram API Key (bot token)

1. Abrir Telegram y buscar `@BotFather`
2. Ejecutar `/newbot` y seguir los pasos para crear el bot
3. Copiar el token que devuelve BotFather
4. En `.env`, configurar:
   - `TELEGRAM_BOT_TOKEN=<tu_token>`
   - `TELEGRAM_CHAT_ID=<tu_chat_id>`

Para obtener tu `chat_id`, escribile un mensaje a tu bot y luego consultá:

- `https://api.telegram.org/bot<TU_TOKEN>/getUpdates`

En la respuesta JSON, el valor `message.chat.id` es el `chat_id` que debés usar.

## Uso

```bash
cabal run tp-arbitraje
```

## Notificaciones de Telegram

El bot envía mensajes cuando detecta oportunidades y al finalizar cada round.

- Mensaje de oportunidad con formato legible para usuario (secciones, saltos de línea y ruta en pasos).
- Ruta mostrada como pares `BASE/QUOTE` (por ejemplo `BTC/USDT`).
- Precisión decimal por activo:
  - `USDT`: 2 decimales
  - Resto de cripto (`BTC`, `ETH`, `BNB`, etc.): 8 decimales

### Comandos disponibles en Telegram

- `/balance` - Muestra balances actuales
- `/status` - Estado general del bot
- `/pnl` - PnL acumulado
- `/open_orders` - Cantidad de órdenes en curso

## Bibliotecas Principales

- `transformers` & `mtl` - Stack monádico
- `aeson` - JSON parsing
- `req` - HTTP client
- `cryptonite` - Criptografía HMAC
- `containers` - Estructuras de datos inmutables
- `dotenv` - Configuración
