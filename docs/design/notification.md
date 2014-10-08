% Retcon Notifications

Most uses of retcon will require some form of human intervention to resolve
conflicts when they occur. This document describes the design of the
notification system. It is currently **draft**.

Requirements
------------

- Notifications are set for all merges with conflicts.

- Notifications are dispatched according to a set of rules.

- Each rule is something like a "report"; it describes the conditions which
will be matched against a notification (entity name, etc.) and the target for
the notification (email address, any aggregation, etc.)

- The system runs periodically, possibly under cron, and processes all
unprocessed notifications according to the current set of rules.

Design
------

The retcon database schema will be extended with a `retcon_notification`
table to store notification details. These details will include at least the
internal key and diff id for the conflicted merge.

The core retcon code will be extended to record the details of conflicted
merges into the `retcon_notification` table.

A new component will be implemented to periodically process this table:
each rule will evaluated against any outstanding notifications, the
results, if any, will be processed and message/s sent according to the
notification parameters.

Implementation
--------------

At each run, the current time stamp will be determined and all operations on
the database will restrict results with this value. This will help to avoid
skipping and double handling of notifications.

Each rule will result in a query (with appropriate parameters) being executed
against the notification table; the results will be processed and messages
dispatched with the target described in the rule.

This will allow a single notification to be sent to multiple destinations.

Individual messages will be dispatched in chronological order; and should use
the timestamp to set the Date header. This will make it easier to inspect
a sequence of events and help understand the operation of the system.

Update the `recordDiffs` operation to update `retcon_notification` when
required.

Queries to select and update the notifications table:

````{.sql}
-- SELECT messages to be processed.
SELECT * FROM retcon_notification
WHERE (processed = FALSE) AND (created < ?) AND (entity = ?)
ORDER BY (entity, datasource) ASC, timestamp DESC

-- UPDATE messages which we just processed.
UPDATE retcon_notification
SET processed = TRUE
WHERE (processed = FALSE) AND (created < ?) AND (entity = ?);
````


