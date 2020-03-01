/* 
  Schema of version 1
*/

/* 
  Scores table
*/
create table "scores" (
  "id" serial not null primary key,
  "username" text not null unique,
  "score" integer default 0, 
  "game_level" integer default 1
);
