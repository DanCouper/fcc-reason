let maxOfList list => List.fold_left (fun acc x => max acc x) 0 list;

let largestOfEach lists => List.map maxOfList;
