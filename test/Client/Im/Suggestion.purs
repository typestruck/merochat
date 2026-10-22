module Test.Client.Im.Suggestion where

import Prelude

import Client.Im.Suggestion as CIS
import Shared.Availability (Availability(..))
import Shared.Backer.Contact (backerId, backerUser)
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests ∷ TestSuite
tests = TU.suite "Suggestion sorting" do
      TU.test "Empty suggestions stay empty" do
            TUA.equal [] $ map _.id $ CIS.byAvailability []
      TU.test "Sorting does not introduce the support card" do
            TUA.equal [ 1, 2 ] $ map _.id $ CIS.byAvailability [ offline, online ]
      TU.test "An existing support card stays last" do
            TUA.equal [ 1, 2, backerId ] $ map _.id $ CIS.byAvailability [ backerUser, offline, online ]
      TU.test "Sorting preserves the existing support card data" do
            TUA.equal [ "Updated support" ] $ map _.headline $ CIS.byAvailability [ backerUser { headline = "Updated support" } ]
      where
      online = backerUser { id = 1, availability = Online }
      offline = backerUser { id = 2, availability = None }
