# ml_quan_sample_1.csv to ml_quan_sample_4.csv
Those files contain the quantitative sample as described in the paper. Most of the columns are self explanatory.  Nonetheless, there's a short description of some of the columns that we believe may not be very clear.

# ml_quan_sample_answers.csv
This file contains answer posts to the question posts found in the quantitative sample. Please note that not all questions have an **accepted answer**. As such, this file will only contain the answer post to those questions with a marked accepted answer. This is provided to measure the response time.

# web_sample.csv
To measure the response time for web development related posts, a sample was generated. In this file the sample of questions alongside the answer post information can be found.

# Columns to know
- Id: StackOverflow's post id.
- PostTypeId: The type of post, is it a question or a response (answer)?
- ParentId: If the post is a question, this filed will contain "None". If the post is a response, then it would have the id of the question post (i.e., the parent).
- AcceptedAnswerId: If the post is a question, this field will contain the id of the response post that was marked as an accepted answer.
- OwnerUserId: The id of the user who created the post.
- LastEditorUserId: User id of the post's last editor.
- Title: The title of the post (if post is a respone, this field will be "None").
- Tags: The list of tags provided with the post.
- Body: The text of the post.
- Score, ViewCount, FavoriteCount, AnswerCount, CommentCount: StackOverflow's stat metrics.
- CreationDate: When was this post created?
- CommunityOwnedDate, LastEditDate, ClosedDate, LastActivityDate: StackOverflow special case dates.
- UserIdCombined: This column was custom made. It will contain the OwnerUserId, if one is found, otherwise, it will use the LastEditorUserId. In many questions, the original author deleted his account, so another user from the community took over, as such, this column should always provide a user id.


