Dataset for ESEM2019 paper ‘‘Why is Developing Machine Learning Applications Challenging? A Study on Stack Overflow Posts’’.

First, the quantitative sample as described in the paper. The four files ml_quan_sample_1.csv to ml_quan_sample_4.csv consititute the sample (). The following information is available in those files:
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


