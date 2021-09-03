# Example 03 - Ticker

## Description

This example shows a ticker of cryptocurrencies, consuming Binance's websockets
API.

Why were cryptocurrencies used for the example? The objective was showing a
real time streaming API and, to make the example easy to use, that API needed to
be free to use and not require user registration. No other available service
satisfied those constraints with the same amount of constantly changing data.

## Preview

![Example gif preview](images/03_Ticker.gif)

## Interesting bits

On startup a channel is created. It is used to send subscribe/unsubscribe
requests to the thread handling the communication with the remote API.

During the `TickerInit` event, a `Producer` is launched. This producer, which
runs on a separate thread from the application the same way `Task` instances do,
performs these steps:

- Connects to Binance's websockets API.
- Launches a new thread for receiving ticker data. A separate thread is required
  because the producer also needs to receive messages from the application.
- Subscribes to the initial ticker list.
- Waits for application messages on its initial thread.

From that point on:

- When a message from the application is received, it is formatted and forwarded
  to the server.
- When a new message from the server is received, it is sent to a grouping
  thread that, every half a second, sends the accumulated messages into the
  application using the provided `sendMsg` function. Since updates for each coin
  are received as independent messages from the server, feeding each of them
  directly into the application would trigger multiple model updates. Grouping
  these messages and only updating the model a few times per second provides
  better and more predictable performance.

The `TickerIgnore` event is used in Tasks that process errors and don't
currently feed information into the application. In general you will want to
report these errors to the user, but logging them may be enough at prototyping
time.
