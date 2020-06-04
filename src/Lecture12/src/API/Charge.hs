module API.Charge where

import Servant.API
import DB.Booking (BookingId)

type ChargeAPI
    = "api" :> "checkout"
        :> Capture "id" BookingId
        :> Get '[JSON] String
        :<|>
        ("api" :> "refund"
        :> Capture "id" BookingId
        :> Get '[JSON] String)