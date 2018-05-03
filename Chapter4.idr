import Data.Vect

data PowerSource = Petrol | Pedal | Electricity

data Vehicle : PowerSource -> Type where
    Bicycle : Vehicle Pedal
    Tram : Vehicle Electricity
    ECar : Vehicle Electricity
    Car : (fuel : Nat) -> Vehicle Petrol
    Bus : (fuel : Nat) -> Vehicle Petrol
    Unicycle : Vehicle Pedal
    Motorcycle : (fuel : Nat) -> Vehicle Petrol


total wheels : Vehicle p -> Nat 
wheels Tram = 8
wheels ECar = 4
wheels Unicycle = 1
wheels (Motorcycle fuel) = 2
wheels Bicycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4

total refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 200
refuel (Bus fuel) = Bus 300
refuel (Motorcycle fuel) = Motorcycle 100

vectTake : (n : Nat) -> Vect (n + m) elem -> Vect n elem
vectTake Z xs = []
vectTake (S k) (x :: xs) = x :: vectTake k xs

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = case integerToFin pos n of
                                Nothing => Nothing
                                (Just x) => Just $ index x xs + index x ys


