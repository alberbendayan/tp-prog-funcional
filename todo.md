# TODO de alineación con la propuesta




## Notificaciones de operatoria
- [x] Enriquecer Telegram con resultado post-trade (`RoundResult`): estado final, PnL, órdenes/fills.
- [x] Enviar también errores de ejecución de órdenes a Telegram (no solo decisión/oportunidad).

## Estado del bot
- [x] Revisar el modelo de estado para incluir balances y órdenes abiertas si se quiere alinear con el texto original.
- [x] Definir métricas mínimas en estado (rondas, PnL acumulado, errores por ronda) y exponerlas en reporte/log.
- [ ] Persistir `BotState` entre iteraciones del ciclo principal (no reiniciar en cada ronda).

## Manejo de errores (ExceptT)
- [x] Unificar estrategia de errores: qué va por `ExceptT BotError` y qué queda como `RoundPartial`.
- [x] Aplicar `throwError` en fallos críticos (conectividad/API), manteniendo parciales para fallos recuperables.

## Dependencias e informe
- [ ] Revisar `.cabal`: agregar `transformers` explícito si el informe/materia lo exige formalmente.
- [ ] Alinear la redacción del informe con la implementación final para evitar desfasajes.

## Consulta periódica
- [ ] Implementar consulta periódica en `app/Main.hs` (loop con intervalo configurable, por ejemplo `threadDelay` + `forever`).
- [ ] Agregar configuración de frecuencia (`N` segundos) por env/config y documentar valor por defecto.

## Datos de mercado (orderbooks vs top of book)
- [ ] Aclarar en documentación que hoy se usa `BookTicker` (top of book) y no profundidad completa.
- [ ] (Opcional) Si se requiere cumplir literal "orderbooks", integrar endpoint de profundidad y adaptar lógica.

## Estado consultable y comandos (Opcional)
- [x] Agregar snapshot de estado en `BotState` (balances por asset, órdenes abiertas y último `RoundResult`).
- [ ] Persistir historial acotado en memoria (por ejemplo últimas `N` rondas) para evitar crecimiento sin límite.
- [ ] Exponer comando de Telegram `/balance` (o equivalente) con balances actuales y variación reciente.
- [ ] Exponer comando `/status` con estado del bot (activo, último ciclo, último error, uptime).
- [ ] Exponer comando `/pnl` con PnL del día/acumulado y cantidad de rondas ejecutadas.
- [ ] Exponer comando `/open_orders` con resumen de órdenes abiertas (símbolo, lado, qty, precio, estado).