module Test.Server.Model where

import Data.Maybe (Maybe(..))
import Server.Types (StorageDetails)

baseUser :: _
baseUser = {
    email: "a@a.com" ,
    name: "Name",
    password: "password",
    headline: "headline",
    description: "description"
}

storageDetails :: StorageDetails
storageDetails = {
    accountAuthorizationToken: Nothing,
    uploadAuthorizationToken: Nothing,
    uploadUrl: Nothing,
    authenticationKey: "",
    apiUrl: Nothing
}