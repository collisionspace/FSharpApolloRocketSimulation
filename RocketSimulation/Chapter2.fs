
module Sim =
(*
  Our simple math module contains basic units of measure and a definition for bidimensional vectors (we will simulate the motion in 2 dimension).
  *)
  module Math =
    (*
    We define a new type with the "type" keyword. The simplest types we can define are types with no definition, which are just placeholders.
    Such types are defined as units of measure, and are preceded by the [<Measure>] attribute.

    We define m (meters), kilograms (kg) and seconds (s).
    *)
    [<Measure>]
    type m

    [<Measure>]
    type kg

    [<Measure>]
    type s

    (*
    Units of measure may be composite. For example, we could define newtons (N) as the unit of measure for force, that is mass times acceleration or kg * m / s^2
    *)
    [<Measure>]
    type N  = kg * m / s^2

    (*
    Units of measure are very simple types, but they do not serve much porpose apart from classifying values of the same type into different groups; for example, we could
    try and sum 2.0<m> + 1.0<s> and the F# compiler would give us an error.

    A more interesting type definition is that of records. Records are very similar to tuples, but with a main difference: the elements (fields) of a record have a name.
    We could, of course, define a vector as float * float. Unfortunately, it would be very easy to make the mistake of losing track of which element is the x and which is the y.
    A record does exactly this: it keeps track of "which element of the tuple is which".

    Any definition of a type can contain a unit of measure, right after its name, between angle brackets. This means that such a type will need to be handled by also giving its values a unit of measure.
    We could avoid doing so, but for a complex physics simulation the advantage of having the compiler track what is in meters and what is in meters per second is quite important.
    
    A simple record definition would be:

    type Vector2 = { X : float; Y : float }

    where a record with units of measure would be defined as follows. Notice that the fields of the record are not simple floating point numbers, but rather they are floating point numbers with the same unit of measure of the record itself:
    *)
    type Vector2<[<Measure>] 'a> =
      {
        X : float<'a>
        Y : float<'a>
      }
      (* 
      A record may have members, which are functions that perform computations on values of type record.
      The simplest members of a record are static.
      Static members are defined with the syntax:
      static member Name : TYPE = BODY
      
      A very useful static member is the zero vector, which returns us a vector with all components initialized to zero.
      Notice that this static member must return a vector with a generic unit of measure 'a ('a may be any valid unit of measure: m, kg, s, m / s, etc.).
      For this reason the fields of the returned record must have the same unit of measure of the vector, as per the above definition.
     
      We invoke a static member by writing, for example:
      let v = Vector2<m/s>.Zero

      where v : Vector2<m/s>      
      *)     
      static member Zero : Vector2<'a> = { X = 0.0<_>; Y = 0.0<_> }

      (*
      We may sum two vectors, as long as they have the same units of measure. To sum them, we build a new record where each field is the sum of the two records.
      We may also sum a vector and a scalar.

      Given two vectors v1 : Vector2<m> and v2 : Vector2<m> we sum them by writing v1 + v2. The result will have type Vector2<m>.
      *)
      static member ( + ) (v1:Vector2<'a>,v2:Vector2<'a>):Vector2<'a> = { X = v1.X + v2.X; Y = v1.Y + v2.Y }
      static member ( + ) (v:Vector2<'a>,k:float<'a>):Vector2<'a> = { X = v.X + k; Y = v.Y + k }
      static member ( + ) (k:float<'a>,v:Vector2<'a>):Vector2<'a> = v + k
      
      (*
      We may negate a vector v by writing -v; this operation is called ~-; in general, an operator prefixed with ~ is a unary operator, that is it takes just one parameter.
      Negating a vector with unit of measure 'a returns another vector with the same unit of measure:
      *)
      static member ( ~- ) (v:Vector2<'a>):Vector2<'a> = { X = -v.X; Y = -v.Y }
      
      (*
      We may subtract vectors and scalars:
      *)
      static member ( - ) (v1:Vector2<'a>,v2:Vector2<'a>):Vector2<'a> = v1 + (-v2)
      static member ( - ) (v:Vector2<'a>,k:float<'a>):Vector2<'a> = v + (-k)
      static member ( - ) (k:float<'a>,v:Vector2<'a>):Vector2<'a> = k + (-v)
 
      (*
      We may multiply vectors and scalars. Multiplying a vector with unit of measure 'a with another vector (or scalar) with another unit of measure 'b produces a vector with unit of measure <'a * 'b>.
      For example, if v1 : Vector2<m> and v2 : Vector2<kg> then v1 * v2 : Vector2<m * kg>:
      *)
      static member ( * ) (v1:Vector2<'a>,v2:Vector2<'b>):Vector2<'a * 'b> = { X = v1.X * v2.X; Y = v1.Y * v2.Y }
      static member ( * ) (v:Vector2<'a>,f:float<'b>):Vector2<'a * 'b> = { X = v.X * f; Y = v.Y * f }
      static member ( * ) (f:float<'b>,v:Vector2<'a>):Vector2<'b * 'a> = { X = f * v.X; Y = f * v.Y }
      (*
      We may divide a vector by a scalar. The resulting vector will have a unit of measure that is the ratio of the units of measure of the vector and the scalar.
      For example, if v : Vector2<m> and t : float<s> then v / s : Vector2<m/s>:
      *)
      static member ( / ) (v:Vector2<'a>,f:float<'b>):Vector2<'a / 'b> = v * (1.0 / f)

      (*
      We can define members that can be applied on a particular vector. These members are called instance members, because they require a valid instance of the type to be invoked.
      Instance members are defined with the syntax:
      member SELF.NAME : TYPE = BODY

      Programmers with an object-oriented background may be used to using the name "this" for SELF.
      A very useful member for our vector computes the length of a vector. Notice that the result is a floating point number with the same unit of measure of the original vector.
      To compute the length of a vector v, we simply write v.Length:
      *)
      member this.Length : float<'a> = sqrt((this.X * this.X + this.Y * this.Y))

      (*
      We use vector subtraction and the length member to define the static member that computes the distance between two vectors and the normalization of a vector:
      *)
      static member Distance(v1:Vector2<'a>,v2:Vector2<'a>) = (v1-v2).Length
      static member Normalize(v:Vector2<'a>):Vector2<1> = v / v.Length

      member this.Normalized = this / this.Length

      static member Dot(v1:Vector2<'a>,v2:Vector2<'a>) = v1.X * v2.X + v1.Y * v2.Y


  module RocketSimulation =

    (*
    We import the System and System.Threading modules (respectively for printing to the console and pausing the application).
    *)
    open System
    open System.Threading

    (*
    We import the Math module we defined above; now we will be able to use the m, kg, s and N units of measure, and also the Vector2 record.
    *)
    open Math

    (*
    We define a physical entity as a position (a vector2 in meters) and a mass (in kilograms).
    *)
    type PhysicalEntity =
      {
        Position : Vector2<m>
        Mass     : float<kg>
      }

    (*
    A celestial body is, simply, a physical entity and an associated name for printing it to screen.
    *)
    type CelestialBody =
      {
        Body     : PhysicalEntity
        Name     : string
      }

    (*
    A stage of our rocket contains its dry mass (in kilograms: the mass it has when empty of fuel), its current amount of fuel (in kilograms), the amount of fuel it burns (in kilograms per second) and the force it produces to push the rocket.
    *)
    type Stage =
      {
        DryMass  : float<kg>
        Fuel     : float<kg>
        FuelBurn : float<kg/s>
        Thrust   : float<N>
      }
      (*
      The total mass of the stage is the sum of its dry mass and the mass of its contained fuel.
      *)
      member this.Mass = this.DryMass + this.Fuel

    (*
    The rocket is defined as a physical entity, with the mass of the base module, a velocity and the three stages. Notice that the stages have type Option<Stage> rather than just Stage. Option<T> is the type for values which may either be empty (None) or which may contain a value of type T (Some x, where x : T); we will use pattern matching to discriminate the two possible values of an Option<T>. We use Option<Stage> because when a stage is used up then it is detached from the body of the rocket, and so we represent it with the None value.
    *)    
    type Rocket =
      {
        Body     : PhysicalEntity
        BaseMass : float<kg>
        Velocity : Vector2<m/s>
        Stage1   : Option<Stage>
        Stage2   : Option<Stage>
        Stage3   : Option<Stage>
      }

    (*
    The state of the simulation is defined as the Earth, the Moon and the rocket.
    *)
    type ApolloMission =
      {
        Earth    : CelestialBody
        Moon     : CelestialBody
        Rocket   : Rocket
      }

    (*
    The simulation uses three constants: the radius of the Earth (to ensure that the rocket does not fall inside the planet at the beginning of the simulation), the amount of time we simulate at each tick and the gravitational constant.
    *)
    let earth_radius = 6.37e6<m>
    let dt = 60.0<s>
    let G = 6.67e-11<m^3 * kg^-1 * s^-2>

    (*
    The initial state of the simulation puts the Earth on the origin, the Moon at the appropriate distance (along the X axis) and the rocket, with all the stages set up, on the surface of the Earth.
    *)
    let m0 =
         {
           Earth =
             {
               Body = { Position = { X = 0.0<m>; Y = 0.0<m> }; Mass = 5.97e24<kg> }
               Name = "E"
             }
           Moon =
             {
               Body = { Position = { X = earth_radius * 60.0; Y = 0.0<m> }; Mass = 7.35e22<kg> }
               Name = "M"
             }
           Rocket =
             (*
             To initialize the rocket, we start by defining its stages.
             Notice that we may use intermediate let-bindings inside a record field initialization.
             *)
             let stage1 =
                   {
                     DryMass     = 1.31e5<kg>
                     Fuel        = 2.17e6<kg>
                     FuelBurn    = 8.25e3<kg/s>
                     Thrust      = 3.4e7<N> * 5.0
                   }
             let stage2 =
                   {
                     DryMass     = 3.6e4<kg>
                     Fuel        = 4.4e5<kg>
                     FuelBurn    = 1.05e3<kg/s>
                     Thrust      = 4.4e6<N> * 5.0
                   }
             let stage3 =
                   {
                     DryMass     = 1.1e4<kg>
                     Fuel        = 1.09e5<kg>
                     FuelBurn    = 2.59e2<kg/s>
                     Thrust      = 1.0e6<N>
                   }
             (*
             The base mass of the rocket without its stages is base_mass.
             *)
             let base_mass = 4.5e4<kg>
             (*
             Now that we have all the components of the rocket set up, we may create it.
             Notice that we have the various stages (stage1, stage2, stage3) but they all have type Stage, and the rocket needs them of type Option<Stage>.
             For this reason we wrap them with the constructor Some, wich takes a value of any type 'a and returns another value of type Option<'a>:
             *)
             {
               Body     = { Position = { X = earth_radius; Y = 11.0<m> }; Mass = base_mass + stage1.Mass + stage2.Mass + stage3.Mass }
               BaseMass = base_mass
               Velocity = Vector2<m/s>.Zero
               Stage1   = Some(stage1)
               Stage2   = Some(stage2)
               Stage3   = Some(stage3)
             }
         }

    (*
    A step of the simulation is a function that takes as input the state of the mission and returns as output the state of the mission after dt seconds have passed.
    *)
    let simulation_step (m:ApolloMission) =
      (*
      We start by extracting the current value of the rocket, and we call it "r" for convenience.
      *)
      let r = m.Rocket

      (*
      We define a helper function that computes the effect of gravity on our rocket "r" from a celestial body "b".
      We use the known formula G * m1 * m2 / r^2, with one notable difference: since we are working with two dimensional vectors, we multiply by the direction vector from one body to the other and we divide by the power of three of the distance between the two bodies:
      *)
      let F_body (b:CelestialBody) =
        let dir = b.Body.Position - r.Body.Position
        let dist = dir.Length + 1.0<m>
        G * b.Body.Mass * r.Body.Mass * dir / (dist * dist * dist)

      (*
      We compute the force that the engines of the rocket produce given the current stage. We also compute the new rocket where part of the fuel of the current stage has been spent:
      *)
      let F_engine,r =
        (*
        We start with a helper function which, given a stage s, computes the new value for that stage.
        If the stage s has burnt up all its fuel, then the stage is detached from the rocket, and so its new value will be None.
        If the stage still has fuel to burn then its value will be Some s' where s' is equal to s with less fuel:
        *)
        let stage_step s =
          if s.Fuel <= s.FuelBurn * dt then
            Console.Beep()
            None
          else
            Some({ s with Fuel = s.Fuel - s.FuelBurn * dt })

        (*
        We compute the direction in which the rocket needs to accelerate to go to the moon:
        *)
        let dir = Vector2<_>.Normalize(m.Moon.Body.Position - r.Body.Position)

        (*
        We check all the stages, looking for the first stage not yet spent; we use pattern matching,
        and the first stage we find we burn up some of its fuel but we also get some thrust in exchange.
        Notice that the result of this pattern matching is assigned to the pair F_engine and r a few lines above:
        *)
        match r.Stage1,r.Stage2,r.Stage3 with
        | Some s,_,_ -> dir * s.Thrust, { r with Stage1 = stage_step s }
        | _,Some s,_ -> dir * s.Thrust, { r with Stage2 = stage_step s }
        | _,_,Some s -> dir * s.Thrust, { r with Stage3 = stage_step s }
        | _ -> Vector2<N>.Zero, r

      (*
      The total amount of force F our rocket is subjected is the sum of the gravitational forces of the Earth and Moon, plus the force coming from the engine:
      *)
      let F =
        let F_earth = (F_body m.Earth)
        let F_moon = F_body m.Moon
        F_earth + F_moon + F_engine

      (*
      At this point we may compute the final value of our rocket r.
      *)
      let r =
          (*
          We define a small helper function that computes the current mass of each stage, by checking if the stage is still attached (Some s) and by adding the dry mass of the stage and the amount of fuel the stage still contains:
          *)
          let stage_mass =
            function
            | None -> 0.0<_>
            | Some s -> s.DryMass + s.Fuel
          (*
          The updated rocket has a different position (it has to be moved of its velocity multiplied by dt) and a different velocity (it has to be accelerated by the current force):
          *)
          {
            r
            with Body =
                   {
                     r.Body
                       with Position = let p = r.Body.Position + r.Velocity * dt 
                                       { p with X = max (p.X) earth_radius };
                            Mass = r.BaseMass + stage_mass r.Stage1 + stage_mass r.Stage2 + stage_mass r.Stage3 };
                 Velocity = r.Velocity + (F / r.Body.Mass) * dt }

      (*
      The new state of the mission is the same initial state m with the new rocket:
      *)
      {
        m with Rocket = r
      }


    (*
    We define another imperative function to print the scene.
    This function uses the Console.SetCursorPosition method to draw only those characters needed, rather than drawing long lines of spaces.
    *)
    let print_scene (m:ApolloMission) =
      (*
      We start by clearing the console and drawing a frame of asterisks around the scene.
      *)
      do Console.Clear()
      for i = 0 to 79 do
        Console.SetCursorPosition(i, 0)
        Console.Write("*")
        Console.SetCursorPosition(i, 23)
        Console.Write("*")
      for j = 0 to 23 do
        Console.SetCursorPosition(0,j)
        Console.Write("*")
        Console.SetCursorPosition(79,j)
        Console.Write("*")
      (*
      We define an auxiliary function to convert a body position into console coordinates (between 80 and 24) and to set the cursor on the converted position.
      *)
      let set_cursor_on_body b =
        Console.SetCursorPosition(((b.Position.X / 4.0e8<m>) * 78.0 + 1.0) |> int, (b.Position.X / 4.0e8<m> + 11.0) |> int)
      (*
      We write the "E" and "M" of the Earth and Moon respectively in their positions, and then we write an "R" for the rocket.
      *)
      do set_cursor_on_body m.Earth.Body
      do Console.Write(m.Earth.Name)
      do set_cursor_on_body m.Moon.Body
      do Console.Write(m.Moon.Name)
      do set_cursor_on_body m.Rocket.Body
      do Console.Write("R")
      (*
      We wait one tenth of a second (100 milliseconds) before the next iteration step, to avoid running the simulation too quickly.
      *)
      do Thread.Sleep(100)

    (*
    The simulation function internally defines and invokes an auxiliary recursive function that continuously prints and updates the state of the simulation, staring from the initial state m0 defined above.
    When the rocket is close enough to the surface of the moon we stop the simulation.
    *)
    let simulation() =
      let rec simulation m =
        do print_scene m
        let m' = simulation_step m
        if Vector2<_>.Distance(m'.Moon.Body.Position,m'.Rocket.Body.Position) > 1.7e6<m> then
          do simulation m'
      do simulation m0

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    Sim.RocketSimulation.simulation()
    0 // return an integer exit code
    
    