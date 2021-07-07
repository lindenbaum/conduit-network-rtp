# TODOs

* Add a DTX stream state, indicating a silence period. The packet rate may drop
  during silence! Silence might begin with the reception of the first comfort
  noise packet, e.g. with payload type 13, see
  https://tools.ietf.org/html/rfc3389 and
  https://tools.ietf.org/html/rfc3551#section-4.1

* split up the received data into equally sized chunks? - leave that to the
  application

* drop duplicate packets

* add parameters: channel layout, bit rate, ptime, maxptime
