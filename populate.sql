COPY profile(username,bio,image)
FROM 'db_dummy/profile.csv'
DELIMITER ','
CSV HEADER;

COPY article(slug,title,description,body,tag_list,created_at,updated_at,author_id)
FROM 'db_dummy/article.csv'
DELIMITER ','
CSV HEADER;
