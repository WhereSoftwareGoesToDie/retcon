CREATE TABLE retcon (id serial NOT NULL, data json);

INSERT INTO retcon (data) VALUES ('{"name": "Thomas Sutton","age" : 30,"address" : {"company" : "Anchor Systems","street" : "Level 11 / 201 Elizabeth Street","locality" : "Sydney","postcode" : 2000,"country" : "Australia"}}');
INSERT INTO retcon (data) VALUES ('{"age" : 30,"address" : {"company" : "Anchor Systems","street" : "Level 11 / 201 Elizabeth Street","locality" : "Sydney","postcode" : 2000,"country" : "Australia"},"quote" : "Never gonna give you up!"}');
