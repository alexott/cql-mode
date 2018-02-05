create keyspace killrvideo with replication = {'class':'SimpleStrategy', 'replication_factor': 1};

use killrvideo;

-- comment

// comment

/*
multi-line comment
*/

drop table if exists videos;

create table if not exists videos (
video_id timeuuid,
added_date timestamp,
description text,
title text,
user_id uuid,
primary key (video_id)
);

truncate videos;
truncate table videos;

COPY videos (video_id, added_date, description, title, user_id)
FROM '/root/labwork/exercise-2/videos.csv'
WITH HEADER = true;


create table videos_by_title_year (
title text,
added_year int,
added_date timestamp,
description text,
user_id uuid,
video_id timeuuid,
primary key ((title, added_year))
);

COPY videos_by_title_year (title, added_year, added_date, description, user_id, video_id)
FROM '/root/labwork/exercise-3/videos_by_title_year.csv'
WITH HEADER = true;

INSERT INTO videos_by_title_year JSON '{"title":"test","added_year":2015}';

INSERT INTO videos_by_title_year (title, added_year) VALUES ('test', 2015);


GRANT SELECT, MODIFY, EXECUTE ON myks TO some_role;

CREATE ROLE security_admin;
drop ROLE security_admin;
GRANT security_admin TO admin_guy;

GRANT AUTHORIZE FOR SELECT, MODIFY, EXECUTE ON patientdata TO security_admin;


RESTRICT SELECT, MODIFY, EXECUTE ON KEYSPACE patientdata TO security_admin;
