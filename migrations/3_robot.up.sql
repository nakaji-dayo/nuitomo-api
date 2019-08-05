insert into "user" values(100, 'robo', 'ぬいぐるみたちに、nuitomoの案内や質問をします', '', '', '', '', '', '', now());

create table user_image (
  id bigint not null,
  user_id bigint references "user"(id) not null,
  path text not null,
  primary key (id)
);

insert into "user_image" values (100, 100, 'system/robot.png');

create table "question" (
  id bigint not null,
  body text not null,
  priority int not null, -- 値が小さい方から順に聞かれていく
  primary key (id)
);

create table "question_user" (
  id bigint not null,
  question_id bigint references "question"(id) not null,
  user_id bigint references "user"(id) not null,
  created_at timestamp not null,
  primary key (id)
);

insert into "question" values (1, 'どこから来たの?', 10);
insert into "question" values (2, 'いつ今のおうちに来たの?', 10);
