# Arbitrín — Bot de Arbitraje Triangular

Bot de arbitraje triangular de criptomonedas desarrollado en Haskell. Detecta y ejecuta oportunidades de arbitraje sobre BTC, ETH, BNB y USDT en Binance Testnet, con notificaciones y comandos interactivos vía Telegram.

## Arquitectura

El sistema está organizado en cinco capas:

| Capa | Módulos | Responsabilidad |
|------|---------|-----------------|
| **Entrada** | `Binance.API.*`, `FakeExchange.*` | Integración con exchanges (real y simulado) |
| **Transformación** | `Bot.Domain`, `Bot.Arbitraje`, `Bot.Pricing`, `Exchange.Interface` | Lógica pura sin efectos |
| **Orquestación** | `Bot.Runtime`, `Bot.Config`, `Bot.Persist`, `Main` | Coordinación, estado y persistencia |
| **Notificación** | `Notification.Types`, `Notification.Telegram` | Envío de alertas proactivas |
| **Comandos** | `Notification.TelegramCommands` | Polling de comandos y respuesta interactiva |

El núcleo de detección (`Bot.Arbitraje`, `Bot.Pricing`) es completamente puro: dado el mismo `MarketSnapshot`, retorna siempre el mismo resultado. Los efectos están confinados al stack monádico `BotM = ReaderT Env (StateT BotState (ExceptT BotError IO))`.

## Setup

### Requisitos

- GHC >= 9.0
- Cabal >= 3.0

### Instalación

```bash
cabal update
cabal build
```

### Configuración

```bash
cp .env.example .env
```

Variables relevantes en `.env`:

```env
# Proveedor: binance | fake (simulador local, sin red)
BOT_EXCHANGE=binance

BINANCE_API_KEY=tu_api_key
BINANCE_API_SECRET=tu_api_secret
BINANCE_BASE_URL=https://testnet.binance.vision

BOT_MIN_PROFIT_PERCENTAGE=0.5       # umbral mínimo de ganancia neta (%)
BOT_MAX_TRADE_AMOUNT_USDT=100       # capital máximo por ronda en USDT

TELEGRAM_BOT_TOKEN=tu_bot_token
TELEGRAM_CHAT_ID=tu_chat_id
TELEGRAM_ENABLED=true

BOT_STATE_FILE=bot_state.json
BOT_POLL_INTERVAL=30                # segundos entre ciclos
```

### Obtener API Keys de Binance Testnet

1. Ir a <https://testnet.binance.vision/>
2. Generar API key y secret
3. Copiar en `.env`

### Obtener token de Telegram

1. Buscar `@BotFather` en Telegram → `/newbot`
2. Copiar el token en `TELEGRAM_BOT_TOKEN`
3. Para obtener tu `chat_id`, enviar un mensaje al bot y consultar:
   ```
   https://api.telegram.org/bot<TOKEN>/getUpdates
   ```
   El campo `message.chat.id` es el valor a usar.

## Uso

```bash
cabal run tp-arbitraje
```

Para correr sin conexión a red (exchange simulado con precios fijos que generan oportunidad rentable):

```bash
# En .env: BOT_EXCHANGE=fake
cabal run tp-arbitraje
```

## Comandos de Telegram

El bot responde a los siguientes comandos en cualquier momento:

| Comando | Descripción |
|---------|-------------|
| `/balance` | Balances reales del último ciclo (BTC, ETH, BNB, USDT) |
| `/status` | Rondas ejecutadas, última ronda, órdenes abiertas, uptime |
| `/pnl` | PnL acumulado por activo desde inicio de sesión |
| `/open_orders` | Cantidad de órdenes en curso |
| `/history [N]` | Últimas N operaciones (default 5, max 20) |

El listener corre en un hilo separado (`forkIO`) con long-polling (`timeout=25s`) y no interfiere con el ciclo principal.

## Lógica de arbitraje

Con cuatro activos (BTC, ETH, BNB, USDT) hay 8 caminos triangulares posibles. El bot los evalúa todos en cada ciclo:

1. **Snapshot**: obtiene precios bid/ask y comisiones reales de la API
2. **Simulación**: calcula retorno neto de cada camino aplicando bid/ask según dirección y comisiones reales. El monto de entrada se acota a la liquidez disponible en el book
3. **Decisión**: selecciona el camino con mayor ganancia neta si supera `BOT_MIN_PROFIT_PERCENTAGE`
4. **Ejecución**: coloca las tres órdenes de mercado en secuencia
5. **PnL real**: convierte los deltas de todos los activos (incluyendo comisiones cobradas en activos intermedios) a USDT usando cotizaciones bid/ask del snapshot (bid para flujos positivos, ask para negativos)

El arbitraje puede partir de cualquier activo con balance positivo (BTC, ETH, BNB o USDT), no solo de USDT.

## Módulos

```
src/
├── Bot/
│   ├── Arbitraje.hs        -- detección pura de oportunidades y liquidity capping
│   ├── Config.hs            -- carga de configuración desde .env
│   ├── Domain.hs            -- tipos del dominio (Asset, Pair, MarketSnapshot, ...)
│   ├── Persist.hs           -- serialización del estado a bot_state.json
│   ├── Pricing.hs           -- conversión de activos a USDT usando bid/ask
│   └── Runtime.hs           -- BotM, Env, BotState, executeRound
├── Binance/API/
│   ├── Auth.hs              -- firma HMAC-SHA256
│   ├── Client.hs            -- llamadas HTTP (ping, bookTicker, tradeFees, placeOrder)
│   ├── Conversion.hs        -- tipos Binance → tipos dominio
│   ├── Endpoints.hs         -- URLs de la API
│   ├── Instance.hs          -- instancia Exchange para Binance
│   └── Types.hs             -- tipos crudos de la API
├── Exchange/
│   ├── AppExchange.hs       -- tipo concreto usado en Main
│   └── Interface.hs         -- typeclass Exchange
├── FakeExchange/
│   ├── Control.hs           -- configuración de precios del simulador
│   └── Instance.hs          -- instancia Exchange simulada (sin red)
└── Notification/
    ├── Telegram.hs          -- sendTelegramMessage y funciones de formateo
    ├── TelegramCommands.hs  -- listener de comandos con long-polling
    └── Types.hs             -- TelegramError
```

## Bibliotecas principales

- `transformers` / `mtl` — stack monádico (`ReaderT`, `StateT`, `ExceptT`)
- `aeson` — parsing/serialización JSON
- `req` — cliente HTTP
- `cryptonite` — firma HMAC-SHA256
- `containers` — `Map` inmutable
- `dotenv` — carga de configuración
