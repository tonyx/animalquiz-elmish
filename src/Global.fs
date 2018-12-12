module Global

type Page =
  | Home
  | AnimalQuiz
  | Counter
  | About

let toHash page =
  match page with
  | About -> "#about"
  | Counter -> "#counter"
  | Home -> "#home"
  | AnimalQuiz -> "#animalQuiz"
