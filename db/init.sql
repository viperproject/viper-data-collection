if db_id('programs') IS NULL
    CREATE DATABASE programs;

GO

USE programs;

GO

create table if not exists `programs`.`ProgramEntries` (`id` BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY,`submissionDate` TIMESTAMP NOT NULL,`originalName` TEXT NOT NULL,`program` TEXT NOT NULL,`loc` INTEGER NOT NULL,`frontend` TEXT NOT NULL,`originalVerifier` TEXT NOT NULL,`siliconResBlob` BLOB NOT NULL,`carbonResBlob` BLOB NOT NULL,`argsBlob` BLOB NOT NULL,`siliconPhaseRuntimesBlob` BLOB NOT NULL,`carbonPhaseRuntimesBlob` BLOB NOT NULL,`programPrintBlob` BLOB NOT NULL,`parseSuccess` BOOLEAN NOT NULL,`hasPreamble` BOOLEAN NOT NULL);
create table if not exists `programs`.`UserSubmissions` (`id` BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY,`submissionDate` TIMESTAMP NOT NULL,`originalName` TEXT NOT NULL,`program` TEXT NOT NULL,`loc` INTEGER NOT NULL,`frontend` TEXT NOT NULL,`argsBlob` BLOB NOT NULL,`originalVerifier` TEXT NOT NULL,`success` BOOLEAN NOT NULL)